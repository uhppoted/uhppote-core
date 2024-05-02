package uhppote

import (
	"fmt"
	"net"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) SetListener(serialNumber uint32, address net.UDPAddr) (*types.Result, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	if address.IP.To4() == nil {
		return nil, fmt.Errorf("invalid IP address: %v", address)
	}

	request := messages.SetListenerRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		Address:      address.IP,
		Port:         uint16(address.Port),
	}

	if reply, err := sendto[messages.SetListenerResponse](u, serialNumber, request); err != nil {
		return nil, err
	} else {
		return &types.Result{
			SerialNumber: reply.SerialNumber,
			Succeeded:    reply.Succeeded,
		}, nil
	}
}
