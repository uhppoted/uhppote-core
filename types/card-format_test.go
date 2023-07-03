package types

import (
	"testing"
)

func TestValidCardFormatFromFromString(t *testing.T) {
	tests := []struct {
		format   string
		expected CardFormat
	}{
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
		if v, err := CardFormatFromString(test.format); err != nil {
			t.Fatalf("error unmarshalling card format (%v)", err)
		} else if v != test.expected {
			t.Errorf("incorrected unmarshalled card format - expected:%#v, got:%#v", test.expected, v)
		}
	}
}

func TestInvalidCardFormatFromString(t *testing.T) {
	tests := []struct {
		format string
	}{
		{""},
		{"none"},
		{"anny"},
		{"Wiegand#26"},
		{"Wiegand-34"},
	}

	for _, test := range tests {
		if _, err := CardFormatFromString(test.format); err == nil {
			t.Fatalf("expeced error unmarshalling invalid card format '%v', got:%v", test.format, err)
		}
	}
}

func TestValidCardFormatUmarshalConf(t *testing.T) {
	tests := []struct {
		format   string
		expected CardFormat
	}{
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

func TestInvalidCardFormatUmarshalConf(t *testing.T) {
	tests := []struct {
		format string
	}{
		{""},
		{"none"},
		{"anny"},
		{"Wiegand#26"},
		{"Wiegand-34"},
	}

	for _, test := range tests {
		var f CardFormat
		var values = map[string]string{
			"card.format": test.format,
		}

		if _, err := f.UnmarshalConf("card.format", values); err == nil {
			t.Fatalf("expeced error unmarshalling invalid card format '%v', got:%v", test.format, err)
		}
	}
}
