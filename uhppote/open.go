package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
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
