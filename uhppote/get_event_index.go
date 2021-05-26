package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetEventIndex(serialNumber uint32) (*types.EventIndex, error) {
	request := messages.GetEventIndexRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	reply := messages.GetEventIndexResponse{}

	err := u.send(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	return &types.EventIndex{
		SerialNumber: reply.SerialNumber,
		Index:        reply.Index,
	}, nil
}
