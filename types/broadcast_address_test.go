package types

import (
	"encoding/json"
	"net/netip"
	"reflect"
	"testing"
)

func TestBroadcastAddrStringer(t *testing.T) {
	tests := map[uint16]string{
		0:     "192.168.1.100:0",
		1:     "192.168.1.100:1",
		60000: "192.168.1.100",
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		broadcast := BroadcastAddrFrom(addr, p)

		if s := broadcast.String(); s != expected {
			t.Errorf("Incorrect string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestParseBroadcastAddr(t *testing.T) {
	tests := map[string]BroadcastAddr{
		"192.168.1.100":       BroadcastAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 60000),
		"192.168.1.100:60000": BroadcastAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 60000),
		"192.168.1.100:10001": BroadcastAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 10001),
		"192.168.1.100:60001": BroadcastAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 60001),
	}

	for s, expected := range tests {
		addr, err := ParseBroadcastAddr(s)

		if err != nil {
			t.Fatalf("Unexpected error resolving broadcast address %v (%v)", s, err)
		} else if !addr.IsValid() {
			t.Fatalf("Failed to resolve valid broadcast address %v (%v)", s, addr)
		}

		if !reflect.DeepEqual(addr, expected) {
			t.Errorf("Incorrectly resolved broadcast address %v - expected:%v, got:%v", s, expected, addr)
		}
	}
}

func TestInvalidBroadcastAddrResolve(t *testing.T) {
	s := "192.168.1.100:0"
	addr, err := ParseBroadcastAddr(s)

	if err == nil {
		t.Errorf("Expected error resolving broadcast address %v, got:%v (%v)", s, addr, err)
	}
}

func TestBroadcastAddrSet(t *testing.T) {
	tests := map[string]BroadcastAddr{
		"192.168.1.100":       MustParseBroadcastAddr("192.168.1.100:60000"),
		"192.168.1.100:12345": MustParseBroadcastAddr("192.168.1.100:12345"),
		"192.168.1.100:60000": MustParseBroadcastAddr("192.168.1.100:60000"),
	}

	for s, expected := range tests {
		addr := BroadcastAddr{}
		err := addr.Set(s)

		if err != nil {
			t.Fatalf("Error 'setting' broadcast address %v (%v)", s, err)
		}

		if !reflect.DeepEqual(addr, expected) {
			t.Errorf("Incorrect 'broadcast' address '%v' - expected:%v, got:%v", s, expected, addr)
		}
	}
}

func TestBroadcastMarshalJSON(t *testing.T) {
	tests := map[uint16]string{
		0:     `"192.168.1.100:0"`,
		1:     `"192.168.1.100:1"`,
		60000: `"192.168.1.100"`,
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		broadcast := BroadcastAddrFrom(addr, p)

		if bytes, err := json.Marshal(broadcast); err != nil {
			t.Fatalf("Error marshaling BroadcastAddr (%v)", err)
		} else if s := string(bytes); s != expected {
			t.Errorf("Incorrect JSON string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestBroadcastAddrUnmarshalJSON(t *testing.T) {
	tests := map[string]BroadcastAddr{
		`"192.168.1.100"`:       MustParseBroadcastAddr("192.168.1.100:60000"),
		`"192.168.1.100:12345"`: MustParseBroadcastAddr("192.168.1.100:12345"),
		`"192.168.1.100:60000"`: MustParseBroadcastAddr("192.168.1.100:60000"),
	}

	for s, expected := range tests {
		broadcast := BroadcastAddr{}

		if err := json.Unmarshal([]byte(s), &broadcast); err != nil {
			t.Fatalf("Error unmarshaling BroadcastAddr '%v' (%v)", s, err)
		} else if !reflect.DeepEqual(broadcast, expected) {
			t.Errorf("Incorrectly unmarshalled broadcast address '%v'\nexpected:%v\ngot:     %v", s, expected, broadcast)
		}
	}
}

func TestBroadcastAddrEqual(t *testing.T) {
	tests := []struct {
		broadcast BroadcastAddr
		address   BroadcastAddr
		expected  bool
	}{
		{
			MustParseBroadcastAddr("192.168.1.100:60000"),
			MustParseBroadcastAddr("192.168.1.100:60000"),
			true,
		},
		{
			MustParseBroadcastAddr("192.168.1.100:60000"),
			MustParseBroadcastAddr("192.168.1.100:12345"),
			true,
		},
		{
			MustParseBroadcastAddr("192.168.1.100:60000"),
			MustParseBroadcastAddr("192.168.1.125:60000"),
			false,
		},
	}

	for _, test := range tests {
		equal := test.broadcast.Equal(&test.address)

		if equal != test.expected {
			t.Errorf("Error comparing broadcast address %v - expected:%v, got:%v", test.address, test.expected, equal)
		}
	}
}

func TestBroadcastAddrClone(t *testing.T) {
	broadcast := MustParseBroadcastAddr("192.168.1.100:12345")
	expected := MustParseBroadcastAddr("192.168.1.100:12345")

	clone := broadcast.Clone()
	broadcast = MustParseBroadcastAddr("192.168.1.100:54321")

	if !reflect.DeepEqual(*clone, expected) {
		t.Errorf("Invalid BroadcastAddr clone\nexpected:%#v\ngot:     %#v", expected, clone)
	}

	if reflect.DeepEqual(broadcast, expected) {
		t.Errorf("Sanity check failed")
	}
}

func TestBroadcastAddrAddr(t *testing.T) {
	broadcast := MustParseBroadcastAddr("192.168.1.100:12345")
	expected := netip.AddrFrom4([4]byte{192, 168, 1, 100})

	addr := broadcast.Addr()

	if !reflect.DeepEqual(addr, expected) {
		t.Errorf("BindAddress::Addr returned incorrect address - expected:%v, got:%v", expected, addr)
	}
}

func TestBroadcastAddrPort(t *testing.T) {
	broadcast := MustParseBroadcastAddr("192.168.1.100:12345")
	expected := uint16(12345)

	port := broadcast.Port()

	if port != expected {
		t.Errorf("BroadcastAddr::Port returned incorrect port - expected:%v, got:%v", expected, port)
	}
}
