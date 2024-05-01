package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) SetDoorControlState(serialNumber uint32, door uint8, state types.ControlState, delay uint8) (*types.DoorControlState, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.SetDoorControlStateRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		Door:         door,
		ControlState: uint8(state),
		Delay:        delay,
	}

	if reply, err := sendto[messages.SetDoorControlStateResponse](u, serialNumber, request); err != nil {
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
