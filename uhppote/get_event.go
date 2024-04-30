package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetEvent(deviceID, index uint32) (*types.Event, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.GetEventRequest{
		SerialNumber: types.SerialNumber(deviceID),
		Index:        index,
	}

	if reply, err := u.sendTo(deviceID, request, messages.GetEventResponse{}); err != nil {
		return nil, err
	} else {
		response := reply.(messages.GetEventResponse)

		if response.Type == 0xff {
			return nil, fmt.Errorf("event at index %v has been overwritten", index)
		}

		if response.Index == 0 {
			return nil, nil
		}

		//	if response.Timestamp.IsZero() == nil {
		//		return nil, fmt.Errorf("invalid 'timestamp' in response")
		//	}

		return &types.Event{
			SerialNumber: response.SerialNumber,
			Index:        response.Index,
			Type:         response.Type,
			Granted:      response.Granted,
			Door:         response.Door,
			Direction:    response.Direction,
			CardNumber:   response.CardNumber,
			Timestamp:    response.Timestamp,
			Reason:       response.Reason,
		}, nil
	}
}
