package uhppote

import (
	"fmt"
	"net"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetListener(serialNumber uint32) (*types.Listener, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.GetListenerRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	if reply, err := sendto[messages.GetListenerResponse](u, serialNumber, request); err != nil {
		return nil, err
	} else {
		return &types.Listener{
			SerialNumber: reply.SerialNumber,
			Address:      net.UDPAddr{IP: reply.Address, Port: int(reply.Port)},
		}, nil
	}
}
