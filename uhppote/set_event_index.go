package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) SetEventIndex(deviceID, index uint32) (*types.EventIndexResult, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.SetEventIndexRequest{
		SerialNumber: types.SerialNumber(deviceID),
		Index:        index,
		MagicWord:    0x55aaaa55,
	}

	reply := messages.SetEventIndexResponse{}

	err := u.send(deviceID, request, &reply)
	if err != nil {
		return nil, err
	}

	return &types.EventIndexResult{
		SerialNumber: reply.SerialNumber,
		Index:        index,
		Changed:      reply.Changed,
	}, nil
}
