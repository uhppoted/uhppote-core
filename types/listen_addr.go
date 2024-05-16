package types

import (
	"encoding/json"
	"fmt"
	"net/netip"
	"regexp"
)

type ListenAddr struct {
	netip.AddrPort
}

/*
 * MustParseListenAddr invokes ParseListenAddr and panics on error.
 *
 * It is intended for use in tests with hard-coded strings.
 */
func MustParseListenAddr(s string) ListenAddr {
	if addr, err := ParseListenAddr(s); err != nil {
		panic(err)
	} else {
		return addr
	}
}

/*
 * ParseListenAddr parses a string as a UDP listen address.
 *
 * It doesn't do any name resolution and the port is required - both the address and the port must be numeric
 */
func ParseListenAddr(s string) (ListenAddr, error) {
	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}:[0-9]{1,5}`, s); err != nil {
		return ListenAddr{}, err
	} else if matched {
		if addr, err := netip.ParseAddrPort(s); err != nil {
			return ListenAddr{}, err
		} else if addr.Port() == 0 || addr.Port() == CONTROLLER_PORT {
			return ListenAddr{}, fmt.Errorf("%v: invalid UDP 'listen' port (%v)", addr, addr.Port())
		} else {
			return ListenAddr{addr}, nil
		}
	}

	return ListenAddr{}, fmt.Errorf("%s is not a valid UDP listen address:port", s)
}

/*
 * ListenAddrFrom contructs a ListenAddr from an address and port.
 */
func ListenAddrFrom(addr netip.Addr, port uint16) ListenAddr {
	return ListenAddr{
		netip.AddrPortFrom(addr, port),
	}
}

/*
 * Formats the listen address and port as an address:port string.
 *
 */
func (a ListenAddr) String() string {
	if !a.IsValid() {
		return ""
	} else {
		return fmt.Sprintf("%v", a.AddrPort)
	}
}

func (a *ListenAddr) Set(v string) error {
	if addr, err := ParseListenAddr(v); err != nil {
		return err
	} else if !addr.IsValid() {
		return fmt.Errorf("invalid broadcast address '%v'", v)
	} else {
		*a = addr

		return nil
	}
}

func (a ListenAddr) MarshalJSON() ([]byte, error) {
	return json.Marshal(fmt.Sprintf("%v", a))
}

func (a *ListenAddr) UnmarshalJSON(bytes []byte) error {
	var s string

	if err := json.Unmarshal(bytes, &s); err != nil {
		return err
	} else if addr, err := ParseListenAddr(s); err != nil {
		return err
	} else {
		*a = addr

		return nil
	}
}

func (a ListenAddr) IsValid() bool {
	return a.AddrPort.Addr().IsValid() && a.AddrPort.Port() != 0
}

func (a *ListenAddr) Equal(addr *ListenAddr) bool {
	switch {
	case a == nil && addr == nil:
		return true

	case a != nil && addr != nil:
		p := fmt.Sprintf("%v", a.Addr())
		q := fmt.Sprintf("%v", addr.Addr())
		return p == q

	default:
		return false
	}
}

func (a *ListenAddr) Clone() *ListenAddr {
	if a != nil {
		addr := *a
		return &addr
	}

	return nil
}
