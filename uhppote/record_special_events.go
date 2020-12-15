package uhppote

import (
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) RecordSpecialEvents(deviceID uint32, enable bool) (bool, error) {
	return recordSpecialEvents(u, deviceID, enable)
}

func recordSpecialEvents(u iuhppote, deviceID uint32, enable bool) (bool, error) {
	request := messages.RecordSpecialEventsRequest{
		SerialNumber: types.SerialNumber(deviceID),
		Enable:       enable,
	}

	reply, err := u.Send(deviceID, request)
	if err != nil {
		return false, err
	}

	response, ok := reply.(*messages.RecordSpecialEventsResponse)
	if !ok {
		return false, fmt.Errorf("Invalid response to RecordSpecialEvents")
	}

	if uint32(response.SerialNumber) != deviceID {
		return false, fmt.Errorf("Incorrect device ID in response - expected '%v', received '%v'", deviceID, response.SerialNumber)
	}

	return response.Succeeded, nil
}
