package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

// Sends a RestoreDefaultParameters request to the designated controller, to reset the
// controller to the manufacturere default configuration.
//
// Returns true if the controller configuration was successfully reset, false if the request
// failed internally to the controller for any reason. Returns an error if the request could
// not be sent or the response is invalid.
func (u *uhppote) RestoreDefaultParameters(deviceID uint32) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.RestoreDefaultParametersRequest{
		SerialNumber: types.SerialNumber(deviceID),
		MagicWord:    0x55aaaa55,
	}

	reply := messages.RestoreDefaultParametersResponse{}

	if err := u.send(deviceID, request, &reply); err != nil {
		return false, err
	} else {
		return reply.Succeeded, nil
	}
}
