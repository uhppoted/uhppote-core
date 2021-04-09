package types

import (
	"fmt"
	"testing"
)

func TestVersionStringer(t *testing.T) {
	tests := []struct {
		version Version
		s       string
	}{
		{Version(0x0662), "v6.62"},
		{Version(0x0892), "v8.92"},
		{Version(0x0898), "v8.98"},
		{Version(0x0800), "v8.00"},
		{Version(0x0801), "v8.01"},
		{Version(0x0810), "v8.10"},
		{Version(0x1234), "v12.34"},
	}

	for _, v := range tests {
		s := fmt.Sprintf("%v", v.version)

		if s != v.s {
			t.Errorf("%#v: Invalid string value - expected:%s, got:%s", v.version, v.s, s)
		}
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
