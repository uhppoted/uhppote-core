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

	if reply, err := u.sendTo(deviceID, request, messages.GetEventIndexResponse{}); err != nil {
		return nil, err
	} else {
		response := reply.(messages.GetEventIndexResponse)

		return &types.EventIndex{
			SerialNumber: response.SerialNumber,
			Index:        response.Index,
		}, nil
	}
}
