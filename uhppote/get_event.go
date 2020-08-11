package uhppote

import (
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) GetEvent(serialNumber, index uint32) (*types.Event, error) {
	return getEvent(u, serialNumber, index)
}

func getEvent(u iuhppote, serialNumber, index uint32) (*types.Event, error) {
	request := messages.GetEventRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		Index:        index,
	}

	reply := messages.GetEventResponse{}

	err := u.Execute(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	if reply.Index == 0 {
		return nil, nil
	}

	if reply.Timestamp == nil {
		return nil, fmt.Errorf("Invalid 'timestamp' in response")
	}

	return &types.Event{
		SerialNumber: reply.SerialNumber,
		Index:        reply.Index,
		Type:         reply.Type,
		Granted:      reply.Granted,
		Door:         reply.Door,
		Direction:    reply.Direction,
		CardNumber:   reply.CardNumber,
		Timestamp:    *reply.Timestamp,
		Reason:       reply.Reason,
	}, nil
}
