package types

import (
	"testing"
)

func TestCardFormatUmarshalConf(t *testing.T) {
	tests := []struct {
		format   string
		expected CardFormat
	}{
		{"", WiegandAny},
		{"any", WiegandAny},
		{" any", WiegandAny},
		{"any ", WiegandAny},
		{"ANY", WiegandAny},
		{"  aNy  ", WiegandAny},
		{"Wiegand-26", Wiegand26},
		{"wiegand-26", Wiegand26},
		{"wiegand26", Wiegand26},
		{"wiegand 26", Wiegand26},
	}

	for _, test := range tests {
		var f CardFormat
		var values = map[string]string{
			"card.format": test.format,
		}

		if v, err := f.UnmarshalConf("card.format", values); err != nil {
			t.Fatalf("error unmarshalling card format (%v)", err)
		} else if v != test.expected {
			t.Errorf("incorrected unmarshalled card format - expected:%#v, got:%#v", test.expected, v)
		} else if f != test.expected {
			t.Errorf("incorrected unmarshalled card format - expected:%v, got:%v", test.expected, f)
		}
	}
}
