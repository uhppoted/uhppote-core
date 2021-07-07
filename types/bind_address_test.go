package types

import (
	"encoding/json"
	"reflect"
	"testing"
)

func TestBindAddrString(t *testing.T) {
	tests := map[int]string{
		0:     "192.168.1.100",
		1:     "192.168.1.100:1",
		60000: "192.168.1.100:60000",
	}

	for p, expected := range tests {
		bind := BindAddr{
			IP:   []byte{192, 168, 1, 100},
			Port: p,
		}

		if s := bind.String(); s != expected {
			t.Errorf("Incorrect string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestBindAddrResolve(t *testing.T) {
	tests := map[string]BindAddr{
		"192.168.1.100":       BindAddr{IP: []byte{192, 168, 1, 100}, Port: 0},
		"192.168.1.100:0":     BindAddr{IP: []byte{192, 168, 1, 100}, Port: 0},
		"192.168.1.100:10001": BindAddr{IP: []byte{192, 168, 1, 100}, Port: 10001},
		"192.168.1.100:60001": BindAddr{IP: []byte{192, 168, 1, 100}, Port: 60001},
	}

	for s, expected := range tests {
		addr, err := ResolveBindAddr(s)

		if err != nil {
			t.Fatalf("Unexpected error resolving bind address %v (%v)", s, err)
		} else if addr == nil {
			t.Fatalf("Failed to resolve valid bind address %v (%v)", s, addr)
		}

		if !reflect.DeepEqual(*addr, expected) {
			t.Errorf("Incorrectly resolved bind address %v - expected:%v, got:%v", s, expected, *addr)
		}
	}
}

func TestInvalidBindAddrResolve(t *testing.T) {
	s := "192.168.1.100:60000"
	addr, err := ResolveBindAddr(s)

	if err == nil {
		t.Errorf("Expected error resolving bind address %v, got:%v (%v)", s, addr, err)
	}
}

func TestBindAddrSet(t *testing.T) {
	tests := map[string]BindAddr{
		"192.168.1.100":       BindAddr{IP: []byte{192, 168, 1, 100}, Port: 0},
		"192.168.1.100:0":     BindAddr{IP: []byte{192, 168, 1, 100}, Port: 0},
		"192.168.1.100:1":     BindAddr{IP: []byte{192, 168, 1, 100}, Port: 1},
		"192.168.1.100:60001": BindAddr{IP: []byte{192, 168, 1, 100}, Port: 60001},
	}

	for s, expected := range tests {
		addr := BindAddr{}
		err := addr.Set(s)

		if err != nil {
			t.Fatalf("Error 'setting' bind address %v (%v)", s, err)
		}

		if !reflect.DeepEqual(addr, expected) {
			t.Errorf("Incorrect 'bind' address '%v' - expected:%v, got:%v", s, expected, addr)
		}
	}
}

func TestBindAddrMarshalJSON(t *testing.T) {
	tests := map[int]string{
		0:     `"192.168.1.100"`,
		1:     `"192.168.1.100:1"`,
		60000: `"192.168.1.100:60000"`,
	}

	for p, expected := range tests {
		bind := BindAddr{
			IP:   []byte{192, 168, 1, 100},
			Port: p,
		}

		if bytes, err := json.Marshal(bind); err != nil {
			t.Fatalf("Error marshaling BindAddr (%v)", err)
		} else if s := string(bytes); s != expected {
			t.Errorf("Incorrect JSON string - expected:%v, got:%v", expected, s)
		}
	}

}
