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
