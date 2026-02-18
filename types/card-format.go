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

func CardFormatFromString(v string) (CardFormat, error) {
	if wAny.MatchString(v) {
		return WiegandAny, nil
	} else if w26.MatchString(v) {
		return Wiegand26, nil
	} else {
		return WiegandAny, fmt.Errorf("invalid card format (%v)", v)
	}
}

func (f CardFormat) MarshalConf(tag string) ([]byte, error) {
	return fmt.Appendf(nil, "%v", f), nil
}

func (f *CardFormat) UnmarshalConf(tag string, values map[string]string) (any, error) {
	if v, ok := values[tag]; ok {
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
