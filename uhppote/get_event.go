package uhppote

import (
	"fmt"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) GetEvent(deviceID, index uint32) (*types.Event, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.GetEventRequest{
		SerialNumber: types.SerialNumber(deviceID),
		Index:        index,
	}

	if reply, err := sendto[messages.GetEventResponse](u, deviceID, request); err != nil {
		return nil, err
	} else {
		if reply.Type == 0xff {
			return nil, fmt.Errorf("event at index %v has been overwritten", index)
		}

		if reply.Index == 0 {
			return nil, nil
		}

		//	if reply.Timestamp.IsZero() == nil {
		//		return nil, fmt.Errorf("invalid 'timestamp' in response")
		//	}

		return &types.Event{
			SerialNumber: reply.SerialNumber,
			Index:        reply.Index,
			Type:         reply.Type,
			Granted:      reply.Granted,
			Door:         reply.Door,
			Direction:    reply.Direction,
			CardNumber:   reply.CardNumber,
			Timestamp:    reply.Timestamp,
			Reason:       reply.Reason,
		}, nil
	}
}
