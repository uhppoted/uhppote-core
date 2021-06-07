package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetEventIndex(deviceID uint32) (*types.EventIndex, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("Invalid device ID (%v)", deviceID)
	}

	request := messages.GetEventIndexRequest{
		SerialNumber: types.SerialNumber(deviceID),
	}

	reply := messages.GetEventIndexResponse{}

	err := u.send(deviceID, request, &reply)
	if err != nil {
		return nil, err
	}

	return &types.EventIndex{
		SerialNumber: reply.SerialNumber,
		Index:        reply.Index,
	}, nil
}
