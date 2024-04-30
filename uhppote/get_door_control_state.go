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

	if reply, err := u.sendTo(serialNumber, request, messages.GetDoorControlStateResponse{}); err != nil {
		return nil, err
	} else {
		response := reply.(messages.GetDoorControlStateResponse)

		return &types.DoorControlState{
			SerialNumber: response.SerialNumber,
			Door:         response.Door,
			ControlState: types.ControlState(response.ControlState),
			Delay:        response.Delay,
		}, nil
	}
}
