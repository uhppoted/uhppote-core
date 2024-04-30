package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
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
