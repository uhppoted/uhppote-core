package types

import (
	"encoding/json"
	"net/netip"
	"reflect"
	"testing"
)

func TestParseListenAddr(t *testing.T) {
	tests := map[string]ListenAddr{
		"192.168.1.100:1":     ListenAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 1),
		"192.168.1.100:60001": ListenAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 60001),
	}

	for s, expected := range tests {
		addr, err := ParseListenAddr(s)

		if err != nil {
			t.Fatalf("Unexpected error resolving listen address %v (%v)", s, err)
		} else if !addr.IsValid() {
			t.Fatalf("Failed to resolve valid listen address %v (%v)", s, addr)
		}

		if !reflect.DeepEqual(addr, expected) {
			t.Errorf("Incorrectly resolved listen address %v - expected:%v, got:%v", s, expected, addr)
		}
	}
}

func TestParseListenAddrWithInvalidPort(t *testing.T) {
	tests := []string{
		"192.168.1.100:0",
		"192.168.1.100:60000",
	}

	for _, s := range tests {
		addr, err := ParseListenAddr(s)
		if err == nil {
			t.Errorf("Expected error resolving listen address %v, got:%v (%v)", s, addr, err)
		}
	}
}

func TestListenAddrString(t *testing.T) {
	tests := map[uint16]string{
		1:     "192.168.1.100:1",
		60001: "192.168.1.100:60001",
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		listen := ListenAddrFrom(addr, p)

		if s := listen.String(); s != expected {
			t.Errorf("Incorrect string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestListenAddrStringWithInvalidValue(t *testing.T) {
	listen := ListenAddr{}

	if s := listen.String(); s != "" {
		t.Errorf("Incorrect string - expected:'%v', got:'%v'", "", s)
	}

	listen = ListenAddr{
		netip.AddrPort{},
	}

	if s := listen.String(); s != "" {
		t.Errorf("Incorrect string - expected:'%v', got:'%v'", "", s)
	}
}

func TestListenAddrMarshalJSON(t *testing.T) {
	tests := map[uint16]string{
		0:     `""`,
		1:     `"192.168.1.100:1"`,
		60001: `"192.168.1.100:60001"`,
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		listen := ListenAddrFrom(addr, p)

		if bytes, err := json.Marshal(listen); err != nil {
			t.Fatalf("Error marshaling ListenAddr (%v)", err)
		} else if s := string(bytes); s != expected {
			t.Errorf("Incorrect JSON string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestListenAddrPointerMarshalJSON(t *testing.T) {
	tests := map[uint16]string{
		0:     `""`,
		1:     `"192.168.1.100:1"`,
		60001: `"192.168.1.100:60001"`,
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		listen := ListenAddrFrom(addr, p)

		if bytes, err := json.Marshal(&listen); err != nil {
			t.Fatalf("Error marshaling ListenAddr (%v)", err)
		} else if s := string(bytes); s != expected {
			t.Errorf("Incorrect JSON string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestListenAddrMarshalJSONWithInvalidValue(t *testing.T) {
	listen := ListenAddr{}

	if bytes, err := json.Marshal(listen); err != nil {
		t.Fatalf("Error marshaling ListenAddr (%v)", err)
	} else if s := string(bytes); s != `""` {
		t.Errorf("Incorrect JSON string - expected:'%v', got:'%v'", `""`, s)
	}

	listen = ListenAddr{
		netip.AddrPort{},
	}

	if bytes, err := json.Marshal(listen); err != nil {
		t.Fatalf("Error marshaling ListenAddr (%v)", err)
	} else if s := string(bytes); s != `""` {
		t.Errorf("Incorrect JSON string - expected:'%v', got:'%v'", `""`, s)
	}
}

func TestListenAddrUnmarshalJSON(t *testing.T) {
	tests := map[string]ListenAddr{
		`"192.168.1.100:60001"`: MustParseListenAddr("192.168.1.100:60001"),
		`"192.168.1.100:12345"`: MustParseListenAddr("192.168.1.100:12345"),
	}

	for s, expected := range tests {
		listen := ListenAddr{}

		if err := json.Unmarshal([]byte(s), &listen); err != nil {
			t.Fatalf("Error unmarshaling ListenAddr '%v' (%v)", s, err)
		} else if !reflect.DeepEqual(listen, expected) {
			t.Errorf("Incorrectly unmarshalled listen address '%v'\nexpected:%v\ngot:     %v", s, expected, listen)
		}
	}
}

func TestListenAddrIsValid(t *testing.T) {
	tests := []struct {
		address  ListenAddr
		expected bool
	}{
		{MustParseListenAddr("192.168.1.100:60001"), true},
		{ListenAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 0), false},
		{ListenAddr{}, false},
	}

	for _, test := range tests {
		v := test.address.IsValid()

		if v != test.expected {
			t.Errorf("IsValid failed for %v - expected:%v, got:%v", test.address, test.expected, v)
		}
	}
}
