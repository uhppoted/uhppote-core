package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

/*
 * Sends a RecordSpecialEvents request to the designated controller, to enable or
 * disable door open, door closed and door button pressed events.
 *
 * Returns true if the controller 'record special events' flag was updated, false
 * if the request failed for any reason. Returns an error if the request could not
 * be sent or the response is invalid.
 */
func (u *uhppote) RecordSpecialEvents(deviceID uint32, enable bool) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.RecordSpecialEventsRequest{
		SerialNumber: types.SerialNumber(deviceID),
		Enable:       enable,
	}

	if reply, err := sendto[messages.RecordSpecialEventsResponse](u, deviceID, request); err != nil {
		return false, err
	} else {
		return reply.Succeeded, nil
	}
}
