package types

import (
	"fmt"
	"testing"
	"time"
)

func TestSystemDateUnmarshalUT0311L0x(t *testing.T) {
	tests := []struct {
		bytes    []byte
		expected SystemDate
		isZero   bool
	}{
		{[]byte{0x21, 0x02, 0x28}, SystemDate(time.Date(2021, time.February, 28, 12, 34, 56, 789, time.Local)), false},
		{[]byte{0x00, 0x00, 0x00}, SystemDate{}, true},
	}

	for _, v := range tests {
		var sysdate SystemDate

		if dt, err := sysdate.UnmarshalUT0311L0x(v.bytes); err != nil {
			t.Errorf("Error unmarshalling %v (%v)", v.bytes, err)
		} else if dt.(*SystemDate).IsZero() != v.isZero {
			t.Errorf("Unmarshalled %v incorrect 'IsZero' - expected:%v, got:%v", v.bytes, v.isZero, dt.(*SystemDate).IsZero())
		} else {
			p := fmt.Sprintf("%v", dt)
			q := fmt.Sprintf("%v", v.expected)

			if p != q {
				t.Errorf("Invalid SystemDate - expected:%v, got:%v", q, p)
			}
		}
	}
}

func TestSystemDateUnmarshalUT0311L0xWithInvalidDateTime(t *testing.T) {
	var bytes = []byte{0x21, 0x02, 0x35}
	var sysdate SystemDate

	if _, err := sysdate.UnmarshalUT0311L0x(bytes); err == nil {
		t.Errorf("Expected error unmarshalling invalid system date, got %v", err)
	}
}
