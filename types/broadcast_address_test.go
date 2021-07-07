package types

import (
	"encoding/json"
	"reflect"
	"testing"
)

func TestBroadcastAddrString(t *testing.T) {
	tests := map[int]string{
		0:     "192.168.1.100:0",
		1:     "192.168.1.100:1",
		60000: "192.168.1.100",
	}

	for p, expected := range tests {
		broadcast := BroadcastAddr{
			IP:   []byte{192, 168, 1, 100},
			Port: p,
		}

		if s := broadcast.String(); s != expected {
			t.Errorf("Incorrect string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestBroadcastAddrResolve(t *testing.T) {
	tests := map[string]BroadcastAddr{
		"192.168.1.100":       BroadcastAddr{IP: []byte{192, 168, 1, 100}, Port: 60000},
		"192.168.1.100:60000": BroadcastAddr{IP: []byte{192, 168, 1, 100}, Port: 60000},
		"192.168.1.100:10001": BroadcastAddr{IP: []byte{192, 168, 1, 100}, Port: 10001},
		"192.168.1.100:60001": BroadcastAddr{IP: []byte{192, 168, 1, 100}, Port: 60001},
	}

	for s, expected := range tests {
		addr, err := ResolveBroadcastAddr(s)

		if err != nil {
			t.Fatalf("Unexpected error resolving broadcast address %v (%v)", s, err)
		} else if addr == nil {
			t.Fatalf("Failed to resolve valid broadcast address %v (%v)", s, addr)
		}

		if !reflect.DeepEqual(*addr, expected) {
			t.Errorf("Incorrectly resolved broadcast address %v - expected:%v, got:%v", s, expected, *addr)
		}
	}
}

func TestInvalidBroadcastAddrResolve(t *testing.T) {
	s := "192.168.1.100:0"
	addr, err := ResolveBroadcastAddr(s)

	if err == nil {
		t.Errorf("Expected error resolving broadcast address %v, got:%v (%v)", s, addr, err)
	}
}

func TestBroadcastMarshalJSON(t *testing.T) {
	tests := map[int]string{
		0:     `"192.168.1.100:0"`,
		1:     `"192.168.1.100:1"`,
		60000: `"192.168.1.100"`,
	}

	for p, expected := range tests {
		broadcast := BroadcastAddr{
			IP:   []byte{192, 168, 1, 100},
			Port: p,
		}

		if bytes, err := json.Marshal(broadcast); err != nil {
			t.Fatalf("Error marshaling BroadcastAddr (%v)", err)
		} else if s := string(bytes); s != expected {
			t.Errorf("Incorrect JSON string - expected:%v, got:%v", expected, s)
		}
	}
}
