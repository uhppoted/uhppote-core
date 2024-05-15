package types

import (
	"encoding/json"
	"net/netip"
	"reflect"
	"testing"
)

func TestControllerAddrString(t *testing.T) {
	tests := map[uint16]string{
		0:     "192.168.1.100:0",
		1:     "192.168.1.100:1",
		60000: "192.168.1.100",
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		controller := ControllerAddrFrom(addr, p)

		if s := controller.String(); s != expected {
			t.Errorf("Incorrect string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestControllerAddrStringWithInvalidValue(t *testing.T) {
	addr := ControllerAddr{}

	if s := addr.String(); s != "" {
		t.Errorf("Incorrect string - expected:'%v', got:'%v'", "", s)
	}

	addr = ControllerAddr{
		netip.AddrPort{},
	}

	if s := addr.String(); s != "" {
		t.Errorf("Incorrect string - expected:'%v', got:'%v'", "", s)
	}
}

func TestParseControllerAddr(t *testing.T) {
	tests := map[string]ControllerAddr{
		"192.168.1.100":       ControllerAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 60000),
		"192.168.1.100:60000": ControllerAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 60000),
		"192.168.1.100:10001": ControllerAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 10001),
		"192.168.1.100:60001": ControllerAddrFrom(netip.AddrFrom4([4]byte{192, 168, 1, 100}), 60001),
	}

	for s, expected := range tests {
		addr, err := ParseControllerAddr(s)

		if err != nil {
			t.Fatalf("Unexpected error resolving controller address %v (%v)", s, err)
		} else if !addr.IsValid() {
			t.Fatalf("Failed to resolve valid controller address %v (%v)", s, addr)
		}

		if !reflect.DeepEqual(addr, expected) {
			t.Errorf("Incorrectly resolved controller address %v - expected:%v, got:%v", s, expected, addr)
		}
	}
}

func TestParseControllerAddrWithInvalidPort(t *testing.T) {
	s := "192.168.1.100:0"
	addr, err := ParseControllerAddr(s)

	if err == nil {
		t.Errorf("Expected error resolving controller address %v, got:%v (%v)", s, addr, err)
	}
}

func TestControllerAddrSet(t *testing.T) {
	tests := map[string]ControllerAddr{
		"192.168.1.100":       MustParseControllerAddr("192.168.1.100:60000"),
		"192.168.1.100:60000": MustParseControllerAddr("192.168.1.100:60000"),
		"192.168.1.100:1":     MustParseControllerAddr("192.168.1.100:1"),
		"192.168.1.100:60001": MustParseControllerAddr("192.168.1.100:60001"),
	}

	for s, expected := range tests {
		addr := ControllerAddr{}
		err := addr.Set(s)

		if err != nil {
			t.Fatalf("Error 'setting' controller address %v (%v)", s, err)
		}

		if !reflect.DeepEqual(addr, expected) {
			t.Errorf("Incorrect 'controller' address '%v' - expected:%v, got:%v", s, expected, addr)
		}
	}
}

func TestControllerAddrMarshalJSON(t *testing.T) {
	tests := map[uint16]string{
		0:     `"192.168.1.100:0"`,
		1:     `"192.168.1.100:1"`,
		60000: `"192.168.1.100"`,
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		controller := ControllerAddrFrom(addr, p)

		if bytes, err := json.Marshal(controller); err != nil {
			t.Fatalf("Error marshaling ControllerAddr (%v)", err)
		} else if s := string(bytes); s != expected {
			t.Errorf("Incorrect JSON string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestControllerAddrPointerMarshalJSON(t *testing.T) {
	tests := map[uint16]string{
		0:     `"192.168.1.100:0"`,
		1:     `"192.168.1.100:1"`,
		60000: `"192.168.1.100"`,
	}

	for p, expected := range tests {
		addr := netip.AddrFrom4([4]byte{192, 168, 1, 100})
		controller := ControllerAddrFrom(addr, p)

		if bytes, err := json.Marshal(&controller); err != nil {
			t.Fatalf("Error marshaling ControllerAddr (%v)", err)
		} else if s := string(bytes); s != expected {
			t.Errorf("Incorrect JSON string - expected:%v, got:%v", expected, s)
		}
	}
}

func TestControllerAddrMarshalJSONWithInvalidValue(t *testing.T) {
	addr := ControllerAddr{}

	if bytes, err := json.Marshal(addr); err != nil {
		t.Fatalf("Error marshaling ControllerAddr (%v)", err)
	} else if s := string(bytes); s != `""` {
		t.Errorf("Incorrect JSON string - expected:'%v', got:'%v'", `""`, s)
	}

	addr = ControllerAddr{
		netip.AddrPort{},
	}

	if bytes, err := json.Marshal(addr); err != nil {
		t.Fatalf("Error marshaling ControllerAddr (%v)", err)
	} else if s := string(bytes); s != `""` {
		t.Errorf("Incorrect JSON string - expected:'%v', got:'%v'", `""`, s)
	}
}

func TestControllerAddrUnmarshalJSON(t *testing.T) {
	tests := map[string]ControllerAddr{
		`"192.168.1.100"`:       MustParseControllerAddr("192.168.1.100:60000"),
		`"192.168.1.100:12345"`: MustParseControllerAddr("192.168.1.100:12345"),
		`"192.168.1.100:60000"`: MustParseControllerAddr("192.168.1.100:60000"),
	}

	for s, expected := range tests {
		controller := ControllerAddr{}

		if err := json.Unmarshal([]byte(s), &controller); err != nil {
			t.Fatalf("Error unmarshaling ControllerAddr '%v' (%v)", s, err)
		} else if !reflect.DeepEqual(controller, expected) {
			t.Errorf("Incorrectly unmarshalled controller address '%v'\nexpected:%v\ngot:     %v", s, expected, controller)
		}
	}
}

func TestControllerAddrEqual(t *testing.T) {
	tests := []struct {
		controller ControllerAddr
		address    ControllerAddr
		expected   bool
	}{
		{
			MustParseControllerAddr("192.168.1.100:60000"),
			MustParseControllerAddr("192.168.1.100:60000"),
			true,
		},
		{
			MustParseControllerAddr("192.168.1.100:12345"),
			MustParseControllerAddr("192.168.1.100:12345"),
			true,
		},
		{
			MustParseControllerAddr("192.168.1.100:60000"),
			MustParseControllerAddr("192.168.1.125:60000"),
			false,
		},
	}

	for _, test := range tests {
		equal := test.controller.Equal(&test.address)

		if equal != test.expected {
			t.Errorf("Error comparing controller address %v - expected:%v, got:%v", test.address, test.expected, equal)
		}
	}
}

func TestControllerAddrClone(t *testing.T) {
	controller := MustParseControllerAddr("192.168.1.100:12345")
	expected := MustParseControllerAddr("192.168.1.100:12345")

	clone := controller.Clone()
	controller = MustParseControllerAddr("192.168.1.100:54321")

	if !reflect.DeepEqual(*clone, expected) {
		t.Errorf("Invalid BindControllerAddr clone\nexpected:%#v\ngot:     %#v", expected, clone)
	}

	if reflect.DeepEqual(controller, expected) {
		t.Errorf("Sanity check failed")
	}
}

func TestControllerAddrAddr(t *testing.T) {
	controller := MustParseControllerAddr("192.168.1.100:12345")
	expected := netip.AddrFrom4([4]byte{192, 168, 1, 100})

	addr := controller.Addr()

	if !reflect.DeepEqual(addr, expected) {
		t.Errorf("BindControllerAddr::Addr returned incorrect address - expected:%v, got:%v", expected, addr)
	}
}

func TestControllerAddrPort(t *testing.T) {
	controller := MustParseControllerAddr("192.168.1.100:12345")
	expected := uint16(12345)

	port := controller.Port()

	if port != expected {
		t.Errorf("BindControllerAddr::Port returned incorrect port - expected:%v, got:%v", expected, port)
	}
}
