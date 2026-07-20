package uhppote

import (
	"fmt"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) OpenDoor(deviceID uint32, door uint8) (*types.Result, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.OpenDoorRequest{
		SerialNumber: types.SerialNumber(deviceID),
		Door:         door,
	}

	if reply, err := sendto[messages.OpenDoorResponse](u, deviceID, request); err != nil {
		return nil, err
	} else {
		return &types.Result{
			SerialNumber: reply.SerialNumber,
			Succeeded:    reply.Succeeded,
		}, nil
	}
}
