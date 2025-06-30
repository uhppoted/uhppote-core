package types

import (
	"encoding/json"
	"fmt"
	"net/netip"
	"regexp"
)

type BindAddr struct {
	netip.AddrPort
}

const BIND_PORT = 0

/*
 * MustParseBindAddr invokes ParseBindAddr and panics on error.
 *
 * It is intended for use in tests with hard-coded strings.
 */
func MustParseBindAddr(s string) BindAddr {
	if addr, err := ParseBindAddr(s); err != nil {
		panic(err)
	} else {
		return addr
	}
}

/*
 * ParseBindAddr parses a string as a bind address.
 *
 * It doesn't do any name resolution i.e.: both the address and the port must be numeric. Defaults
 * to port 0 if the port is not specified.
 */
func ParseBindAddr(s string) (BindAddr, error) {
	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}:[0-9]{1,5}`, s); err != nil {
		return BindAddr{}, err
	} else if matched {
		if addr, err := netip.ParseAddrPort(s); err != nil {
			return BindAddr{}, err
		} else if addr.Port() == CONTROLLER_PORT {
			return BindAddr{}, fmt.Errorf("%v: invalid 'bind' port (%v)", addr, addr.Port()) // avoid broadcast-to-self
		} else {
			return BindAddr{addr}, nil
		}
	}

	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}`, s); err != nil {
		return BindAddr{}, err
	} else if matched {
		if addr, err := netip.ParseAddr(s); err != nil {
			return BindAddr{}, err
		} else {
			return BindAddrFrom(addr, BIND_PORT), nil
		}
	}

	return BindAddr{}, fmt.Errorf("%s is not a valid bind address:port", s)
}

/*
 * BindAddrFrom contructs a BindAddr from an address and port.
 */
func BindAddrFrom(addr netip.Addr, port uint16) BindAddr {
	return BindAddr{
		netip.AddrPortFrom(addr, port),
	}
}

/*
 * Formats the bind address and port as an address:port string.
 *
 * Return only the bind address if bind port is the default port (60000).
 */
func (a BindAddr) String() string {
	if !a.IsValid() {
		return ""
	} else if a.Port() == BIND_PORT {
		return fmt.Sprintf("%v", a.Addr())
	} else {
		return fmt.Sprintf("%v", a.AddrPort)
	}
}

func (a *BindAddr) Set(v string) error {
	if addr, err := ParseBindAddr(v); err != nil {
		return err
	} else if !addr.IsValid() {
		return fmt.Errorf("invalid bind address '%v'", v)
	} else {
		*a = addr

		return nil
	}
}

func (a BindAddr) MarshalJSON() ([]byte, error) {
	return json.Marshal(fmt.Sprintf("%v", a))
}

func (a *BindAddr) UnmarshalJSON(bytes []byte) error {
	var s string

	if err := json.Unmarshal(bytes, &s); err != nil {
		return err
	}

	if addr, err := ParseBindAddr(s); err != nil {
		return err
	} else {
		*a = addr

		return nil
	}
}

func (a *BindAddr) Equal(addr *ControllerAddr) bool {
	switch {
	case a == nil && addr == nil:
		return true

	case a != nil && addr != nil:
		return a.Addr() == addr.Addr()

	default:
		return false
	}
}

func (a *BindAddr) Clone() *BindAddr {
	if a != nil {
		addr := *a
		return &addr
	}

	return nil
}
