package UTO311_L0x

import (
	"encoding/binary"
	"fmt"
	"net"
	"reflect"
	"regexp"
	"strconv"

	"github.com/uhppoted/uhppote-core/types"
)

type Marshaler interface {
	MarshalUT0311L0x() ([]byte, error)
}

type Unmarshaler interface {
	UnmarshalUT0311L0x([]byte) (interface{}, error)
}

var (
	tBool    = reflect.TypeOf(bool(false))
	tByte    = reflect.TypeOf(byte(0))
	tUint16  = reflect.TypeOf(uint16(0))
	tUint32  = reflect.TypeOf(uint32(0))
	tIPv4    = reflect.TypeOf(net.IPv4(0, 0, 0, 0))
	tMAC     = reflect.TypeOf(net.HardwareAddr{})
	tSOM     = reflect.TypeOf(types.SOM(0))
	tMsgType = reflect.TypeOf(types.MsgType(0))
)

var re = regexp.MustCompile(`offset:\s*([0-9]+)`)
var vre = regexp.MustCompile(`value:\s*((?:0[xX])?[0-9a-fA-F]+)`)

func Marshal(m interface{}) ([]byte, error) {
	v := reflect.ValueOf(m)
	bytes := make([]byte, 64)

	bytes[0] = 0x17

	if v.Type().Kind() == reflect.Ptr {
		if err := marshal(v.Elem(), bytes); err != nil {
			return nil, err
		}
	} else {
		if err := marshal(reflect.Indirect(v), bytes); err != nil {
			return nil, err
		}
	}

	return bytes, nil
}

func marshal(s reflect.Value, bytes []byte) error {
	if s.Kind() == reflect.Struct {
		N := s.NumField()

		for i := 0; i < N; i++ {
			f := s.Field(i)
			t := s.Type().Field(i)

			if t.Anonymous {
				if err := marshal(f, bytes); err != nil {
					return err
				}
			} else {
				tag := t.Tag.Get("uhppote")

				switch t.Type {
				case tSOM:
					if value := vre.FindStringSubmatch(tag); value == nil {
						bytes[0] = byte(f.Uint())
					} else {
						if v, err := strconv.ParseUint(value[1], 0, 8); err != nil {
							return err
						} else {
							bytes[0] = byte(v)
						}
					}

				case tMsgType:
					if value := vre.FindStringSubmatch(tag); value == nil {
						bytes[1] = byte(f.Uint())
					} else {
						if v, err := strconv.ParseUint(value[1], 0, 8); err != nil {
							return err
						} else {
							bytes[1] = byte(v)
						}
					}

				default:
					if matched := re.FindStringSubmatch(tag); matched != nil {
						offset, _ := strconv.Atoi(matched[1])

						// Marshall with MarshalUT0311L0x{} interface
						if m, ok := f.Interface().(Marshaler); ok {
							// If f is a pointer type and the value is nil skips this field, leaving the buffer 'as is'
							if f.Kind() != reflect.Ptr || !f.IsNil() {
								if b, err := m.MarshalUT0311L0x(); err == nil {
									copy(bytes[offset:offset+len(b)], b)
								}
							}

							continue
						}

						// Marshal built-in types
						switch t.Type {
						case tByte:
							value := vre.FindStringSubmatch(tag)
							if value != nil {
								v, err := strconv.ParseUint(value[1], 16, 8)
								if err != nil {
									return err
								}
								bytes[offset] = byte(v)
							} else {
								bytes[offset] = byte(f.Uint())
							}

						case tUint16:
							binary.LittleEndian.PutUint16(bytes[offset:offset+4], uint16(f.Uint()))

						case tUint32:
							binary.LittleEndian.PutUint32(bytes[offset:offset+4], uint32(f.Uint()))

						case tBool:
							if f.Bool() {
								bytes[offset] = 0x01
							} else {
								bytes[offset] = 0x00
							}

						case tIPv4:
							copy(bytes[offset:offset+4], f.MethodByName("To4").Call([]reflect.Value{})[0].Bytes())

						case tMAC:
							copy(bytes[offset:offset+6], f.Bytes())

						default:
							panic(fmt.Errorf("cannot marshal field with type '%v'", t.Type))
						}
					}
				}
			}
		}
	}

	return nil
}

func Unmarshal(bytes []byte, m interface{}) error {
	v := reflect.ValueOf(m)

	if v.Kind() == reflect.Ptr && v.Elem().Kind() == reflect.Struct {
		return unmarshal(bytes, v.Elem())
	}

	return fmt.Errorf("cannot unmarshal value with kind '%s'", v.Type())
}

func UnmarshalAs(bytes []byte, m interface{}) (interface{}, error) {
	v := reflect.ValueOf(m)

	if v.Kind() != reflect.Ptr && v.Kind() == reflect.Struct {
		s := reflect.New(v.Type()).Elem()
		if err := unmarshal(bytes, s); err != nil {
			return nil, err
		}

		return s.Interface(), nil
	}

	if v.Kind() == reflect.Ptr && v.Elem().Kind() == reflect.Struct {
		s := reflect.New(v.Elem().Type()).Elem()
		if err := unmarshal(bytes, s); err != nil {
			return nil, err
		}

		return s.Interface(), nil
	}

	return nil, fmt.Errorf("cannot unmarshal value with kind '%s'", v.Type())
}

func UnmarshalArray(bytes [][]byte, array interface{}) error {
	v := reflect.ValueOf(array)

	if v.Kind() == reflect.Ptr && v.Elem().Kind() == reflect.Slice {
		t := v.Elem().Type().Elem()
		vv := reflect.MakeSlice(reflect.SliceOf(t), 0, 0)

		for _, b := range bytes {
			s := reflect.New(t).Elem()
			if err := unmarshal(b, s); err != nil {
				return err
			}

			vv = reflect.Append(vv, s)
		}

		v.Elem().Set(vv)

		return nil
	}

	return fmt.Errorf("cannot unmarshal array to value with kind '%s'", v.Type())
}

func UnmarshalArrayElement(bytes []byte, array interface{}) (interface{}, error) {
	v := reflect.ValueOf(array)

	if v.Kind() == reflect.Ptr && v.Elem().Kind() == reflect.Slice {
		t := v.Elem().Type().Elem()
		s := reflect.New(t).Elem()
		if err := unmarshal(bytes, s); err != nil {
			return nil, err
		}

		return s.Interface(), nil
	}

	return nil, fmt.Errorf("cannot unmarshal array to value with kind '%s'", v.Type())
}

func unmarshal(bytes []byte, s reflect.Value) error {
	// Validate message format
	if len(bytes) != 64 {
		return fmt.Errorf("invalid message length - expected 64 bytes, received %v", len(bytes))
	}

	// PATCH: v6.62 firmware sends 'listen' events with SOM 0x19
	if (bytes[0] != 0x17) && (bytes[0] != 0x19 || bytes[1] != 0x20) {
		return fmt.Errorf("invalid start of message - expected 0x17, received 0x%02x", bytes[0])
	}

	// Unmarshal fields tagged with `uhppote:"..."`
	if s.Kind() == reflect.Struct {
		N := s.NumField()

		for i := 0; i < N; i++ {
			f := s.Field(i)
			t := s.Type().Field(i)
			tag := t.Tag.Get("uhppote")

			if !f.CanSet() {
				continue
			}

			switch {
			case t.Anonymous: // embedded structs
				unmarshal(bytes, f)

			case t.Type == tMsgType: // validate MsgType field
				b := byte(f.Uint())
				if value := vre.FindStringSubmatch(tag); value != nil {
					if v, err := strconv.ParseUint(value[1], 0, 8); err != nil {
						return err
					} else {
						b = byte(v)
					}
				}

				if bytes[1] != b {
					return fmt.Errorf("invalid MsgType in message - expected %02X, received %02X", b, bytes[1])
				}

				f.SetUint(uint64(bytes[1]))

			default:
				// Unmarshall fields tagged with `uhppote:"offset:<offset>"`
				matched := re.FindStringSubmatch(tag)
				if matched == nil {
					continue
				}

				offset, _ := strconv.Atoi(matched[1])

				// Unmarshall value fields with UnmarshalUT0311L0x{} interface
				if u, ok := f.Addr().Interface().(Unmarshaler); ok {
					if p, err := u.UnmarshalUT0311L0x(bytes[offset:]); err != nil {
						return err
					} else {
						f.Set(reflect.Indirect(reflect.ValueOf(p)))
					}
					continue
				}

				// Unmarshall pointer fields with UnmarshalUT0311L0x{} interface
				if u, ok := f.Interface().(Unmarshaler); ok {
					if p, err := u.UnmarshalUT0311L0x(bytes[offset:]); err == nil && p != nil {
						f.Set(reflect.ValueOf(p))
					}
					continue
				}

				// Unmarshal built-in types
				switch t.Type {
				case tBool:
					if bytes[offset] == 0x01 {
						f.SetBool(true)
					} else if bytes[offset] == 0x00 {
						f.SetBool(false)
					} else {
						return fmt.Errorf("invalid boolean value in message: %02x", bytes[offset])
					}

				case tByte:
					value := vre.FindStringSubmatch(tag)
					if value != nil {
						v, err := strconv.ParseUint(value[1], 16, 8)
						if err != nil {
							return err
						}
						if bytes[offset] != byte(v) {
							return fmt.Errorf("invalid value in message - expected %02x, received 0x%02x", v, bytes[offset])
						}
					}

					f.SetUint(uint64(bytes[offset]))

				case tUint16:
					f.SetUint(uint64(binary.LittleEndian.Uint16(bytes[offset : offset+2])))

				case tUint32:
					f.SetUint(uint64(binary.LittleEndian.Uint32(bytes[offset : offset+4])))

				case tIPv4:
					f.SetBytes(net.IPv4(bytes[offset], bytes[offset+1], bytes[offset+2], bytes[offset+3]))

				case tMAC:
					f.SetBytes(bytes[offset : offset+6])

				default:
					panic(fmt.Errorf("cannot unmarshal field with type '%v'", t.Type))
				}
			}
		}
	}

	return nil
}
