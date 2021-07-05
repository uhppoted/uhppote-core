package types

import (
	"encoding/json"
	"fmt"
	"net"
	"regexp"
)

type BindAddr net.UDPAddr

const BIND_PORT = 0

func (a *BindAddr) String() string {
	if a != nil {
		if a.Port == BIND_PORT {
			return a.IP.String()
		} else {
			return (*net.UDPAddr)(a).String()
		}
	}

	return ""
}

func (a *BindAddr) Set(v string) error {
	addr, err := ResolveBindAddr(v)
	if err != nil {
		return err
	} else if addr == nil {
		return fmt.Errorf("Invalid bind address '%v'", v)
	}

	*a = *addr
	return nil
}

func (a *BindAddr) UnmarshalJSON(bytes []byte) error {
	var s string

	if err := json.Unmarshal(bytes, &s); err != nil {
		return err
	}

	addr, err := ResolveBindAddr(s)
	if err != nil {
		return err
	}

	*a = *addr

	return nil
}

func (a *BindAddr) MarshalConf(tag string) ([]byte, error) {
	s := fmt.Sprintf("%v", a)

	return []byte(s), nil
}

func (a *BindAddr) UnmarshalConf(tag string, values map[string]string) (interface{}, error) {
	if v, ok := values[tag]; ok {
		return ResolveBindAddr(v)
	}

	return nil, nil
}

func (a *BindAddr) Equal(addr *Address) bool {
	switch {
	case a == nil && addr == nil:
		return true

	case a != nil && addr != nil:
		return a.IP.Equal(addr.IP)

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

func ResolveBindAddr(s string) (*BindAddr, error) {
	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}:[0-9]{1,5}`, s); err != nil {
		return nil, err
	} else if matched {
		if addr, err := net.ResolveUDPAddr("udp", s); err != nil {
			return nil, err
		} else if addr.Port == DEFAULT_PORT {
			return nil, fmt.Errorf("%v: invalid 'bind' port (%v)", addr, addr.Port)
		} else {
			return &BindAddr{
				IP:   addr.IP.To4(),
				Port: addr.Port,
			}, nil
		}
	}

	if matched, err := regexp.MatchString(`[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}`, s); err != nil {
		return nil, err
	} else if matched {
		if ip := net.ParseIP(s); ip != nil {
			return &BindAddr{
				IP:   ip.To4(),
				Port: BIND_PORT,
			}, nil
		}
	}

	return nil, fmt.Errorf("%s is not a valid UDP address:port", s)
}
