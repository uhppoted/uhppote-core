package types

import (
	"reflect"
	"testing"
)

func TestAddressString(t *testing.T) {
	tests := map[int]string{
		0:     "192.168.1.100:0",
		1:     "192.168.1.100:1",
		60000: "192.168.1.100",
	}

	for p, expected := range tests {
		address := Address{
			IP:   []byte{192, 168, 1, 100},
			Port: p,
		}

		if s := address.String(); s != expected {
			t.Errorf("Incorrect string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestAddressResolve(t *testing.T) {
	tests := map[string]Address{
		"192.168.1.100":       Address{IP: []byte{192, 168, 1, 100}, Port: 60000},
		"192.168.1.100:60000": Address{IP: []byte{192, 168, 1, 100}, Port: 60000},
		"192.168.1.100:10001": Address{IP: []byte{192, 168, 1, 100}, Port: 10001},
		"192.168.1.100:60001": Address{IP: []byte{192, 168, 1, 100}, Port: 60001},
	}

	for s, expected := range tests {
		addr, err := ResolveAddr(s)

		if err != nil {
			t.Fatalf("Unexpected error resolving address address %v (%v)", s, err)
		} else if addr == nil {
			t.Fatalf("Failed to resolve valid address address %v (%v)", s, addr)
		}

		if !reflect.DeepEqual(*addr, expected) {
			t.Errorf("Incorrectly resolved address address %v - expected:%v, got:%v", s, expected, *addr)
		}
	}
}

func TestInvalidAddressResolve(t *testing.T) {
	s := "192.168.1.100:0"
	addr, err := ResolveAddr(s)

	if err == nil {
		t.Errorf("Expected error resolving address address %v, got:%v (%v)", s, addr, err)
	}
}
