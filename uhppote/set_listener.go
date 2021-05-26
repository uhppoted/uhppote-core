package uhppote

import (
	"errors"
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
	"net"
)

func (u *uhppote) SetListener(serialNumber uint32, address net.UDPAddr) (*types.Result, error) {
	if address.IP.To4() == nil {
		return nil, errors.New(fmt.Sprintf("Invalid IP address: %v", address))
	}

	request := messages.SetListenerRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		Address:      address.IP,
		Port:         uint16(address.Port),
	}

	reply := messages.SetListenerResponse{}

	err := u.send(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	return &types.Result{
		SerialNumber: reply.SerialNumber,
		Succeeded:    reply.Succeeded,
	}, nil
}
