package types

import (
	"encoding/json"
	"fmt"
	"net/netip"
	"regexp"
)

type BroadcastAddr struct {
	netip.AddrPort
}

const BROADCAST_PORT = 60000

/*
 * MustParseBroadcastAddr invokes ParseBroadcastAddr and panics on error.
 *
 * It is intended for use in tests with hard-coded strings.
 */
func MustParseBroadcastAddr(s string) BroadcastAddr {
	if addr, err := ParseBroadcastAddr(s); err != nil {
		panic(err)
	} else {
		return addr
	}
}

/*
 * ParseBroadcastAddr parses a string as a UDP broadcast address.
 *
 * It doesn't do any name resolution i.e.: both the address and the port must be numeric.
 */
func ParseBroadcastAddr(s string) (BroadcastAddr, error) {
	if addr, err := netip.ParseAddrPort(s); err != nil {
		return BroadcastAddr{}, err
	} else {
		return BroadcastAddr{
			addr,
		}, nil
	}
}

/*
 * BroadcastAddrFrom contructs a BroadcastAddr from an address and port.
 */
func BroadcastAddrFrom(addr netip.Addr, port uint16) BroadcastAddr {
	return BroadcastAddr{
		netip.AddrPortFrom(addr, port),
	}
}

/*
 * Formats the broadcast address and port as an address:port string.
 *
 * Return only the bind address if bind port is the default port (60000).
 */
func (a BroadcastAddr) String() string {
	if a.Port() == BROADCAST_PORT {
		return fmt.Sprintf("%v", a.Addr())
	} else {
		return fmt.Sprintf("%v", a.AddrPort)
	}
}

func (a *BroadcastAddr) Set(v string) error {
	addr, err := ResolveBroadcastAddr(v)
	if err != nil {
		return err
	} else if !addr.IsValid() {
		return fmt.Errorf("invalid broadcast address '%v'", v)
	}

	*a = addr

	return nil
}

func (a BroadcastAddr) MarshalJSON() ([]byte, error) {
	return json.Marshal(fmt.Sprintf("%v", a))
}

func (a *BroadcastAddr) UnmarshalJSON(bytes []byte) error {
	var s string

	if err := json.Unmarshal(bytes, &s); err != nil {
		return err
	}

	addr, err := ResolveBroadcastAddr(s)
	if err != nil {
		return err
	}

	*a = addr

	return nil
}

func (a *BroadcastAddr) Equal(addr *BroadcastAddr) bool {
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

func (a *BroadcastAddr) Clone() *BroadcastAddr {
	if a != nil {
		addr := *a
		return &addr
	}

	return nil
}

func ResolveBroadcastAddr(s string) (BroadcastAddr, error) {
	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}:[0-9]{1,5}`, s); err != nil {
		return BroadcastAddr{}, err
	} else if matched {
		if addr, err := netip.ParseAddrPort(s); err != nil {
			return BroadcastAddr{}, err
		} else if addr.Port() == 0 {
			return BroadcastAddr{}, fmt.Errorf("%v: invalid 'broadcast' port (%v)", addr, addr.Port())
		} else {
			return BroadcastAddr{addr}, nil
		}
	}

	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}`, s); err != nil {
		return BroadcastAddr{}, err
	} else if matched {
		if addr, err := netip.ParseAddr(s); err != nil {
			return BroadcastAddr{}, err
		} else {
			return BroadcastAddrFrom(addr, BROADCAST_PORT), nil
		}
	}

	return BroadcastAddr{}, fmt.Errorf("%s is not a valid UDP broadcast address:port", s)
}
