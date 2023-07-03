package types

import (
	"fmt"
	"regexp"
)

type CardFormat uint8

const (
	WiegandAny CardFormat = iota
	Wiegand26
)

var wAny = regexp.MustCompile(`(?i)\s*any\s*`)
var w26 = regexp.MustCompile(`(?i)\s*wiegand([ \-])?26\s*`)

func (f CardFormat) String() string {
	return []string{"any", "Wiegand-26"}[f]
}

func (f CardFormat) MarshalConf(tag string) ([]byte, error) {
	return []byte(fmt.Sprintf("%v", f)), nil
}

func (f *CardFormat) UnmarshalConf(tag string, values map[string]string) (any, error) {
	if v, ok := values[tag]; ok && v != "" {
		if wAny.MatchString(v) {
			*f = WiegandAny
		} else if w26.MatchString(v) {
			*f = Wiegand26
		} else {
			return WiegandAny, fmt.Errorf("invalid card format (%v)", v)
		}
	}

	return *f, nil
}
