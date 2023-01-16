package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetDoorControlState(serialNumber uint32, door byte) (*types.DoorControlState, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.GetDoorControlStateRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		Door:         door,
	}

	reply := messages.GetDoorControlStateResponse{}

	err := u.send(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	return &types.DoorControlState{
		SerialNumber: reply.SerialNumber,
		Door:         reply.Door,
		ControlState: types.ControlState(reply.ControlState),
		Delay:        reply.Delay,
	}, nil
}
