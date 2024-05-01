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
func (u *uhppote) RestoreDefaultParameters(controller uint32) (bool, error) {
	if controller == 0 {
		return false, fmt.Errorf("invalid controller ID (%v)", controller)
	}

	request := messages.RestoreDefaultParametersRequest{
		SerialNumber: types.SerialNumber(controller),
		MagicWord:    0x55aaaa55,
	}

	if reply, err := sendto[messages.RestoreDefaultParametersResponse](u, controller, request); err != nil {
		return false, err
	} else {
		return reply.Succeeded, nil
	}
}
