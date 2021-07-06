package types

import (
	"reflect"
	"testing"
)

func TestListenAddrString(t *testing.T) {
	tests := map[int]string{
		0:     "192.168.1.100:0",
		1:     "192.168.1.100:1",
		60001: "192.168.1.100:60001",
	}

	for p, expected := range tests {
		listen := ListenAddr{
			IP:   []byte{192, 168, 1, 100},
			Port: p,
		}

		if s := listen.String(); s != expected {
			t.Errorf("Incorrect string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestListenAddrResolve(t *testing.T) {
	tests := map[string]ListenAddr{
		"192.168.1.100:1":     ListenAddr{IP: []byte{192, 168, 1, 100}, Port: 1},
		"192.168.1.100:60001": ListenAddr{IP: []byte{192, 168, 1, 100}, Port: 60001},
	}

	for s, expected := range tests {
		addr, err := ResolveListenAddr(s)

		if err != nil {
			t.Fatalf("Unexpected error resolving listen address %v (%v)", s, err)
		} else if addr == nil {
			t.Fatalf("Failed to resolve valid listen address %v (%v)", s, addr)
		}

		if !reflect.DeepEqual(*addr, expected) {
			t.Errorf("Incorrectly resolved listen address %v - expected:%v, got:%v", s, expected, *addr)
		}
	}
}

func TestInvalidListenAddrResolve(t *testing.T) {
	tests := []string{
		"192.168.1.100:0",
		"192.168.1.100:60000",
	}

	for _, s := range tests {
		addr, err := ResolveListenAddr(s)
		if err == nil {
			t.Errorf("Expected error resolving listen address %v, got:%v (%v)", s, addr, err)
		}
	}
}
