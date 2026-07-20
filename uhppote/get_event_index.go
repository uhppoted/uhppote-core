package uhppote

import (
	"fmt"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) GetEventIndex(deviceID uint32) (*types.EventIndex, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.GetEventIndexRequest{
		SerialNumber: types.SerialNumber(deviceID),
	}

	if reply, err := sendto[messages.GetEventIndexResponse](u, deviceID, request); err != nil {
		return nil, err
	} else {
		return &types.EventIndex{
			SerialNumber: reply.SerialNumber,
			Index:        reply.Index,
		}, nil
	}
}
