package types

import (
	"encoding/json"
	"fmt"
	"net/netip"
	"regexp"
)

type ControllerAddr struct {
	netip.AddrPort
}

const CONTROLLER_PORT = 60000

/*
 * MustParseControllerAddr invokes ParseControllerAddr and panics on error.
 *
 * It is intended for use in tests with hard-coded strings.
 */
func MustParseControllerAddr(s string) ControllerAddr {
	if addr, err := ParseControllerAddr(s); err != nil {
		panic(err)
	} else {
		return addr
	}
}

/*
 * ParseControllerAddr parses a string as a controller address.
 *
 * It doesn't do any name resolution i.e.: both the address and the port must be numeric. Defaults
 * to port 60000 if the port is not specified.
 */
func ParseControllerAddr(s string) (ControllerAddr, error) {
	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}:[0-9]{1,5}`, s); err != nil {
		return ControllerAddr{}, err
	} else if matched {
		if addr, err := netip.ParseAddrPort(s); err != nil {
			return ControllerAddr{}, err
		} else if addr.Port() == 0 {
			return ControllerAddr{}, fmt.Errorf("%v: invalid 'controller' port (%v)", addr, addr.Port())
		} else {
			return ControllerAddr{addr}, nil
		}
	}

	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}`, s); err != nil {
		return ControllerAddr{}, err
	} else if matched {
		if addr, err := netip.ParseAddr(s); err != nil {
			return ControllerAddr{}, err
		} else {
			return ControllerAddrFrom(addr, CONTROLLER_PORT), nil
		}
	}

	return ControllerAddr{}, fmt.Errorf("%s is not a valid bind address:port", s)
}

/*
 * ControllerAddrFrom contructs a ControllerAddr from an address and port.
 */
func ControllerAddrFrom(addr netip.Addr, port uint16) ControllerAddr {
	return ControllerAddr{
		netip.AddrPortFrom(addr, port),
	}
}

/*
 * Formats the controller address and port as an address:port string.
 *
 * Return only the controller address if bind port is the default port (60000).
 */
func (a ControllerAddr) String() string {
	if !a.IsValid() {
		return ""
	} else if a.Port() == CONTROLLER_PORT {
		return fmt.Sprintf("%v", a.Addr())
	} else {
		return fmt.Sprintf("%v", a.AddrPort)
	}
}

func (a *ControllerAddr) Set(v string) error {
	if addr, err := ParseControllerAddr(v); err != nil {
		return err
	} else if !addr.IsValid() {
		return fmt.Errorf("invalid controller address '%v'", v)
	} else {
		*a = addr

		return nil
	}
}

func (a ControllerAddr) MarshalJSON() ([]byte, error) {
	return json.Marshal(a.String())
}

func (a *ControllerAddr) UnmarshalJSON(bytes []byte) error {
	var s string

	if err := json.Unmarshal(bytes, &s); err != nil {
		return err
	} else if addr, err := ParseControllerAddr(s); err != nil {
		return err
	} else {
		*a = addr

		return nil
	}
}

func (a *ControllerAddr) Equal(addr *ControllerAddr) bool {
	switch {
	case a == nil && addr == nil:
		return true

	case a != nil && addr != nil:
		return a.Addr() == addr.Addr()

	default:
		return false
	}
}

func (a *ControllerAddr) Clone() *ControllerAddr {
	if a != nil {
		addr := *a
		return &addr
	}

	return nil
}
