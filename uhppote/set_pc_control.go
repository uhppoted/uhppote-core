package uhppote

import (
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

// Sends a SetPCControl request to the designated controller, to enable or
// disable remote access control.
//
// Returns true if the controller remote access control was set, false if
// the request failed for any reason. Returns an error if the request could not
// be sent or the response is invalid.
func (u *UHPPOTE) SetPCControl(deviceID uint32, enable bool) (bool, error) {
	return setPCControl(u, deviceID, enable)
}

// Internal implementation of SetPCControl for test mocks/stubs and facades for
// external interfaces.
func setPCControl(u iuhppote, deviceID uint32, enable bool) (bool, error) {
	request := messages.SetPCControlRequest{
		SerialNumber: types.SerialNumber(deviceID),
		MagicWord:    0x55aaaa55,
		Enable:       enable,
	}

	reply, err := u.Send(deviceID, request)
	if err != nil {
		return false, err
	}

	response, ok := reply.(*messages.SetPCControlResponse)
	if !ok {
		return false, fmt.Errorf("Invalid response to SetPCControl")
	}

	if uint32(response.SerialNumber) != deviceID {
		return false, fmt.Errorf("Incorrect device ID in response - expected '%v', received '%v'", deviceID, response.SerialNumber)
	}

	return response.Succeeded, nil
}
