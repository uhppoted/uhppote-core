package types

import (
	"fmt"
	"regexp"
	"strings"
)

type CardFormat uint8

const (
	WiegandAny CardFormat = iota
	Wiegand26
)

var re = regexp.MustCompile(`[ \-]`)

func (f CardFormat) String() string {
	return []string{"any", "Wiegand-26"}[f]
}

func (f CardFormat) MarshalConf(tag string) ([]byte, error) {
	return []byte(fmt.Sprintf("%v", f)), nil
}

func (f *CardFormat) UnmarshalConf(tag string, values map[string]string) (any, error) {
	if v, ok := values[tag]; ok && v != "" {
		if strings.ToLower(v) == "any" {
			*f = WiegandAny
		} else if strings.ToLower(re.ReplaceAllString(v, "")) == "wiegand26" {
			*f = Wiegand26
		}
	}

	return *f, nil
}
