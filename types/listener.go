package types

import (
	"fmt"
	"net"
)

// FIXME remove (unused)
type Listener struct {
	SerialNumber SerialNumber
	Address      net.UDPAddr
}

func (l *Listener) String() string {
	return fmt.Sprintf("%v %v", l.SerialNumber, l.Address)
}
