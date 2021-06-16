package tsv

import (
	"bytes"
	"encoding/csv"
	"fmt"
	"io"
	"reflect"
	"regexp"
	"strconv"
	"strings"

	"github.com/uhppoted/uhppote-core/types"
)

type Unmarshaler interface {
	UnmarshalTSV(string) (interface{}, error)
}

var (
	tBool    = reflect.TypeOf(bool(false))
	tUint8   = reflect.TypeOf(uint8(0))
	tInt     = reflect.TypeOf(int(0))
	tDate    = reflect.TypeOf(types.Date{})
	tDatePtr = reflect.TypeOf((*types.Date)(nil))
	tHHmm    = reflect.TypeOf(types.HHmm{})
	tHHmmPtr = reflect.TypeOf((*types.HHmm)(nil))
)

func Unmarshal(b []byte, array interface{}) error {
	v := reflect.ValueOf(array)

	if v.Kind() != reflect.Ptr || v.Elem().Kind() != reflect.Slice {
		return fmt.Errorf("Cannot unmarshal TSV to value with kind '%s'", v.Type())
	}

	buffer := bytes.NewBuffer(b)
	r := csv.NewReader(buffer)
	r.Comma = '\t'

	index := map[string]int{}
	if header, err := r.Read(); err != nil {
		return err
	} else {
		for i, v := range header {
			index[clean(v)] = i
		}
	}

	rid := 0
	t := v.Elem().Type().Elem()
	vv := reflect.MakeSlice(reflect.SliceOf(t), 0, 0)

	for {
		if record, err := r.Read(); err == io.EOF {
			break
		} else if err != nil {
			return err
		} else {
			rid++

			s := reflect.New(t).Elem()
			if err := unmarshal(rid, record, index, s); err != nil {
				return err
			}

			vv = reflect.Append(vv, s)
		}
	}

	v.Elem().Set(vv)

	return nil
}

func unmarshal(rid int, record []string, index map[string]int, s reflect.Value) error {
	if s.Kind() == reflect.Struct {
		N := s.NumField()

		for i := 0; i < N; i++ {
			f := s.Field(i)
			t := s.Type().Field(i)
			tag := t.Tag.Get("tsv")

			if !f.CanSet() {
				continue
			}

			// Unmarshall fields tagged with `tsv:"<name>"`
			field := clean(tag)
			ix, ok := index[field]
			if !ok {
				return fmt.Errorf("Required field '%s' not included in TSV", tag)
			} else if ix >= len(record) {
				return fmt.Errorf("Record %v: missing required field '%s'", rid, tag)
			}

			value := strings.TrimSpace(record[ix])

			// Unmarshall value fields with UnmarshalTSV{} interface
			if u, ok := f.Addr().Interface().(Unmarshaler); ok {
				if p, err := u.UnmarshalTSV(value); err != nil {
					return err
				} else {
					f.Set(reflect.Indirect(reflect.ValueOf(p)))
				}
				continue
			}

			// Unmarshall pointer fields with UnmarshalTSV{} interface
			if u, ok := f.Interface().(Unmarshaler); ok {
				if p, err := u.UnmarshalTSV(value); err == nil {
					f.Set(reflect.ValueOf(p))
				}
				continue
			}

			// Unmarshal built-in types
			switch t.Type {
			case tBool:
				v := strings.ToUpper(value)
				switch v {
				case "Y":
					f.SetBool(true)
				case "N":
					f.SetBool(false)
				default:
					return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
				}

			case tUint8:
				if value != "" {
					if v, err := strconv.ParseUint(value, 10, 8); err != nil {
						return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
					} else {
						f.SetUint(v)
					}
				}

			case tInt:
				if value != "" {
					if v, err := strconv.Atoi(value); err != nil {
						return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
					} else {
						f.SetInt(int64(v))
					}
				}

			case tDate:
				if v, err := types.DateFromString(value); err != nil {
					return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
				} else if v == nil {
					return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
				} else {
					f.Set(reflect.ValueOf(*v))
				}

			case tDatePtr:
				if value != "" {
					if v, err := types.DateFromString(value); err != nil {
						return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
					} else if v == nil {
						return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
					} else {
						f.Set(reflect.ValueOf(v))
					}
				}

			case tHHmm:
				if v, err := types.HHmmFromString(value); err != nil {
					return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
				} else if v == nil {
					return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
				} else {
					f.Set(reflect.ValueOf(*v))
				}

			case tHHmmPtr:
				if value != "" {
					if v, err := types.HHmmFromString(value); err != nil {
						return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
					} else if v == nil {
						return fmt.Errorf("Record %v: invalid value '%s' for field '%s'", rid, value, tag)
					} else {
						f.Set(reflect.ValueOf(v))
					}
				}

			default:
				panic(fmt.Errorf("Cannot unmarshal field with type '%v'", t.Type))
			}
		}
	}

	return nil
}

func clean(s string) string {
	return regexp.MustCompile(`[\s\t]+`).ReplaceAllString(strings.ToLower(s), "")
}
