package uhppote

import (
	"fmt"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) GetDoorControlState(serialNumber uint32, door byte) (*types.DoorControlState, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.GetDoorControlStateRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		Door:         door,
	}

	if reply, err := sendto[messages.GetDoorControlStateResponse](u, serialNumber, request); err != nil {
		return nil, err
	} else {
		return &types.DoorControlState{
			SerialNumber: reply.SerialNumber,
			Door:         reply.Door,
			ControlState: types.ControlState(reply.ControlState),
			Delay:        reply.Delay,
		}, nil
	}
}
