package types

import (
	"encoding/json"
	"fmt"
	"net"
	"regexp"
)

type BroadcastAddr net.UDPAddr

const BROADCAST_PORT = 60000

func (a *BroadcastAddr) String() string {
	if a != nil {
		if a.Port == BROADCAST_PORT {
			return a.IP.String()
		} else {
			return (*net.UDPAddr)(a).String()
		}
	}

	return ""
}

func (a *BroadcastAddr) MarshalJSON() ([]byte, error) {
	return json.Marshal(a.String())
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

	*a = *addr

	return nil
}

func (a *BroadcastAddr) Equal(addr *BroadcastAddr) bool {
	switch {
	case a == nil && addr == nil:
		return true

	case a != nil && addr != nil:
		return a.IP.Equal(addr.IP)

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

func ResolveBroadcastAddr(s string) (*BroadcastAddr, error) {
	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}:[0-9]{1,5}`, s); err != nil {
		return nil, err
	} else if matched {
		if addr, err := net.ResolveUDPAddr("udp", s); err != nil {
			return nil, err
		} else if addr.Port == 0 {
			return nil, fmt.Errorf("%v: invalid 'broadcast' port (%v)", addr, addr.Port)
		} else {
			return &BroadcastAddr{
				IP:   addr.IP.To4(),
				Port: addr.Port,
			}, nil
		}
	}

	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}`, s); err != nil {
		return nil, err
	} else if matched {
		if ip := net.ParseIP(s); ip != nil {
			return &BroadcastAddr{
				IP:   ip.To4(),
				Port: BROADCAST_PORT,
			}, nil
		}
	}

	return nil, fmt.Errorf("%s is not a valid UDP address:port", s)
}
