package types

import (
	"encoding/json"
	"fmt"
	"net"
	"regexp"
)

type ListenAddr net.UDPAddr

func (a *ListenAddr) String() string {
	if a != nil {
		return (*net.UDPAddr)(a).String()
	}

	return ""
}

func (a *ListenAddr) MarshalJSON() ([]byte, error) {
	return json.Marshal(a.String())
}

func (a *ListenAddr) UnmarshalJSON(bytes []byte) error {
	var s string

	if err := json.Unmarshal(bytes, &s); err != nil {
		return err
	}

	addr, err := ResolveListenAddr(s)
	if err != nil {
		return err
	}

	*a = *addr

	return nil
}

func (a *ListenAddr) Equal(addr *ListenAddr) bool {
	switch {
	case a == nil && addr == nil:
		return true

	case a != nil && addr != nil:
		return a.IP.Equal(addr.IP)

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

func ResolveListenAddr(s string) (*ListenAddr, error) {
	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}:[0-9]{1,5}`, s); err != nil {
		return nil, err
	} else if matched {
		if addr, err := net.ResolveUDPAddr("udp", s); err != nil {
			return nil, err
		} else if addr.Port == 0 || addr.Port == 60000 {
			return nil, fmt.Errorf("%v: invalid 'listen' port (%v)", addr, addr.Port)
		} else {
			return &ListenAddr{
				IP:   addr.IP.To4(),
				Port: addr.Port,
			}, nil
		}
	}

	return nil, fmt.Errorf("%s is not a valid UDP address:port", s)
}
