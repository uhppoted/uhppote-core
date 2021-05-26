package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) OpenDoor(serialNumber uint32, door uint8) (*types.Result, error) {
	request := messages.OpenDoorRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		Door:         door,
	}

	reply := messages.OpenDoorResponse{}

	err := u.send(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	return &types.Result{
		SerialNumber: reply.SerialNumber,
		Succeeded:    reply.Succeeded,
	}, nil
}
