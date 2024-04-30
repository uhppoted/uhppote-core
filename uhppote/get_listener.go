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

	if reply, err := u.sendTo(serialNumber, request, messages.GetListenerResponse{}); err != nil {
		return nil, err
	} else {
		response := reply.(messages.GetListenerResponse)

		return &types.Listener{
			SerialNumber: response.SerialNumber,
			Address:      net.UDPAddr{IP: response.Address, Port: int(response.Port)},
		}, nil
	}
}
