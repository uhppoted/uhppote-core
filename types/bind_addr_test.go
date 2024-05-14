package types

import (
	"encoding/json"
	"net/netip"
	"reflect"
	"testing"
)

func TestBindAddrString(t *testing.T) {
	tests := map[uint16]string{
		0:     "192.168.1.100",
		1:     "192.168.1.100:1",
		60000: "192.168.1.100:60000",
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		bind := BindAddrFrom(addr, p)

		if s := bind.String(); s != expected {
			t.Errorf("Incorrect string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestParseBindAddr(t *testing.T) {
	tests := map[string]BindAddr{
		"192.168.1.100":       BindAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 0),
		"192.168.1.100:0":     BindAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 0),
		"192.168.1.100:10001": BindAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 10001),
		"192.168.1.100:60001": BindAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 60001),
	}

	for s, expected := range tests {
		addr, err := ParseBindAddr(s)

		if err != nil {
			t.Fatalf("Unexpected error resolving bind address %v (%v)", s, err)
		} else if !addr.IsValid() {
			t.Fatalf("Failed to resolve valid bind address %v (%v)", s, addr)
		}

		if !reflect.DeepEqual(addr, expected) {
			t.Errorf("Incorrectly resolved bind address %v - expected:%v, got:%v", s, expected, addr)
		}
	}
}

func TestParseBindAddrWithInvalidPort(t *testing.T) {
	s := "192.168.1.100:60000"
	addr, err := ParseBindAddr(s)

	if err == nil {
		t.Errorf("Expected error resolving bind address %v, got:%v (%v)", s, addr, err)
	}
}

func TestBindAddrSet(t *testing.T) {
	tests := map[string]BindAddr{
		"192.168.1.100":       MustParseBindAddr("192.168.1.100:0"),
		"192.168.1.100:0":     MustParseBindAddr("192.168.1.100:0"),
		"192.168.1.100:1":     MustParseBindAddr("192.168.1.100:1"),
		"192.168.1.100:60001": MustParseBindAddr("192.168.1.100:60001"),
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
	tests := map[uint16]string{
		0:     `"192.168.1.100"`,
		1:     `"192.168.1.100:1"`,
		60000: `"192.168.1.100:60000"`,
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		bind := BindAddrFrom(addr, p)

		if bytes, err := json.Marshal(bind); err != nil {
			t.Fatalf("Error marshaling BindAddr (%v)", err)
		} else if s := string(bytes); s != expected {
			t.Errorf("Incorrect JSON string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestBindAddrPointerMarshalJSON(t *testing.T) {
	tests := map[uint16]string{
		0:     `"192.168.1.100"`,
		1:     `"192.168.1.100:1"`,
		60000: `"192.168.1.100:60000"`,
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		bind := BindAddrFrom(addr, p)

		if bytes, err := json.Marshal(&bind); err != nil {
			t.Fatalf("Error marshaling BindAddr (%v)", err)
		} else if s := string(bytes); s != expected {
			t.Errorf("Incorrect JSON string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestBindAddrUnmarshalJSON(t *testing.T) {
	tests := map[string]BindAddr{
		`"192.168.1.100"`:       MustParseBindAddr("192.168.1.100:0"),
		`"192.168.1.100:12345"`: MustParseBindAddr("192.168.1.100:12345"),
		`"192.168.1.100:0"`:     MustParseBindAddr("192.168.1.100:0"),
	}

	for s, expected := range tests {
		bind := BindAddr{}

		if err := json.Unmarshal([]byte(s), &bind); err != nil {
			t.Fatalf("Error unmarshaling BindAddr '%v' (%v)", s, err)
		} else if !reflect.DeepEqual(bind, expected) {
			t.Errorf("Incorrectly unmarshalled bind address '%v'\nexpected:%v\ngot:     %v", s, expected, bind)
		}
	}
}

func TestBindAddrEqual(t *testing.T) {
	tests := []struct {
		bind     BindAddr
		address  ControllerAddr
		expected bool
	}{
		{
			MustParseBindAddr("192.168.1.100:0"),
			MustParseControllerAddr("192.168.1.100:60000"),
			true,
		},
		{
			MustParseBindAddr("192.168.1.100:12345"),
			MustParseControllerAddr("192.168.1.100:12345"),
			true,
		},
		{
			MustParseBindAddr("192.168.1.100:0"),
			MustParseControllerAddr("192.168.1.125:60000"),
			false,
		},
	}

	for _, test := range tests {
		equal := test.bind.Equal(&test.address)

		if equal != test.expected {
			t.Errorf("Error comparing bind address %v - expected:%v, got:%v", test.address, test.expected, equal)
		}
	}
}

func TestBindAddrClone(t *testing.T) {
	bind := MustParseBindAddr("192.168.1.100:12345")
	expected := MustParseBindAddr("192.168.1.100:12345")

	clone := bind.Clone()
	bind = MustParseBindAddr("192.168.1.100:54321")

	if !reflect.DeepEqual(*clone, expected) {
		t.Errorf("Invalid BindControllerAddr clone\nexpected:%#v\ngot:     %#v", expected, clone)
	}

	if reflect.DeepEqual(bind, expected) {
		t.Errorf("Sanity check failed")
	}
}

func TestBindAddrAddr(t *testing.T) {
	bind := MustParseBindAddr("192.168.1.100:12345")
	expected := netip.AddrFrom4([4]byte{192, 168, 1, 100})

	addr := bind.Addr()

	if !reflect.DeepEqual(addr, expected) {
		t.Errorf("BindControllerAddr::Addr returned incorrect address - expected:%v, got:%v", expected, addr)
	}
}

func TestBindAddrPort(t *testing.T) {
	bind := MustParseBindAddr("192.168.1.100:12345")
	expected := uint16(12345)

	port := bind.Port()

	if port != expected {
		t.Errorf("BindControllerAddr::Port returned incorrect port - expected:%v, got:%v", expected, port)
	}
}
