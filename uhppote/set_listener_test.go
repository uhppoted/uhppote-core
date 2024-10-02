package uhppote

import (
	"errors"
	"fmt"
	"net"
	"net/netip"
	"testing"
)

func TestSetListener(t *testing.T) {
	message := []byte{
		0x17, 0x90, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	u := uhppote{
		driver: &stub{
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	if ok, err := u.SetListener(405419896, netip.MustParseAddrPort("192.168.1.100:60001"), 13); err != nil {
		t.Errorf("unexpected error (%v)", err)
	} else if !ok {
		t.Errorf("invalid response - expected:%v, got:%v", true, ok)
	}
}

func TestSetListenerWithANYAddr(t *testing.T) {
	message := []byte{
		0x17, 0x90, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	u := uhppote{
		driver: &stub{
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	if ok, err := u.SetListener(405419896, netip.MustParseAddrPort("0.0.0.0:0"), 13); err != nil {
		t.Errorf("unexpected error (%v)", err)
	} else if !ok {
		t.Errorf("invalid response - expected:%v, got:%v", true, ok)
	}
}

func TestSetListenerWithResponseFromIncorrectController(t *testing.T) {
	message := []byte{
		0x17, 0x90, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	u := uhppote{
		driver: &stub{
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	if ok, err := u.SetListener(405419896, netip.MustParseAddrPort("192.168.1.100:60001"), 13); err == nil {
		t.Errorf("expected 'invalid controller' error, got %v", err)
	} else if fmt.Sprintf("%v", err) != "invalid controller ID - expected:405419896, got:423187757" {
		t.Errorf("expected 'woot', got:%v", err)
	} else if ok {
		t.Errorf("expected 'not ok', got %v", ok)
	}
}

func TestSetListenerWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	if _, err := u.SetListener(0, netip.MustParseAddrPort("192.168.1.100:60001"), 13); err == nil {
		t.Errorf("Expected 'invalid device ID' error, got %v", err)
	}
}

func TestSetListenerWithInvalidAddrPort(t *testing.T) {
	u := uhppote{}

	if _, err := u.SetListener(405419896, netip.AddrPort{}, 13); !errors.Is(err, ErrInvalidListenerAddress) {
		t.Errorf("Expected 'invalid listener address' error, got %v", err)
	}
}

func TestSetListenerWithIPv6Address(t *testing.T) {
	u := uhppote{}

	if _, err := u.SetListener(405419896, netip.MustParseAddrPort("[2001:db8::68]:60001"), 13); err == nil {
		t.Errorf("Expected 'invalid listener address: [2001:db8::68]:60001', got %v", err)
	} else if fmt.Sprintf("%v", err) != "invalid listener address: [2001:db8::68]:60001" {
		t.Errorf("Expected 'invalid listener address: [2001:db8::68]:60001', got %v", err)
	}
}

func TestSetListenerWithInvalidPort(t *testing.T) {
	u := uhppote{}

	if _, err := u.SetListener(405419896, netip.MustParseAddrPort("192.168.1.100:0"), 13); err == nil {
		t.Errorf("Expected 'invalid listener address: 192.168.1.100:0', got %v", err)
	} else if fmt.Sprintf("%v", err) != "invalid listener address: 192.168.1.100:0" {
		t.Errorf("Expected 'invalid listener address: 192.168.1.100:0', got %v", err)
	}
}
