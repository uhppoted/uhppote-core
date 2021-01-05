package types

import (
	"fmt"
	"testing"
)

func TestVersionStringer(t *testing.T) {
	v := Version(0x0662)
	s := fmt.Sprintf("%v", v)

	if s != "6.62" {
		t.Errorf("Invalid string value - expected:%s, got:%s", "6.62", s)
	}
}

func TestMarshalJSON(t *testing.T) {
	v := Version(0x0662)
	b, err := v.MarshalJSON()
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if string(b) != `"0662"` {
		t.Errorf("Invalid JSON value - expected:%s, got:%s", `"0662"`, string(b))
	}
}

func TestUnmarshalJSON(t *testing.T) {
	v := Version(0)
	err := v.UnmarshalJSON([]byte(`"0662"`))
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if uint16(v) != 0x0662 {
		t.Errorf("Invalid version - expected:%v, got:%v", "0662", v)
	}
}
