package types

import (
	"fmt"
	"testing"
)

func TestAntiPassbackString(t *testing.T) {
	tests := []struct {
		antipassback AntiPassback
		expected     string
	}{
		{Disabled, "disabled"},
		{Readers12_34, "1:2;3:4"},
		{Readers13_24, "(1,3):(2,4)"},
		{Readers1_23, "1:(2,3)"},
		{Readers1_234, "1:(2,3,4)"},
	}

	for _, v := range tests {
		s := fmt.Sprintf("%v", v.antipassback)

		if s != v.expected {
			t.Errorf("Invalid anti-passback string - expected:%v, got:%v", v.expected, s)
		}
	}
}
