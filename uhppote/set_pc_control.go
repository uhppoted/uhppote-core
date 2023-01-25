package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

// Sends a SetPCControl request to the designated controller, to enable or disable
// remote host control of access.
//
// The access controller expects the host to communicate at least once every 30 seconds
// otherwise it reverts to local control of access using the stored list of cards (the
// communication is not required to be a 'set-pc-control' command - any command is sufficient).
// If the access controller has reverted to local control because no message has been received
// from the host for more than 30 seconds, any subsequent communication from the remote host
// will re-establish remote control again.
//
// Returns true if the controller 'PC control' was successfully enabled or disabled,
// false if the request failed for any reason. Returns an error if the request could not
// be sent or the response is invalid.
func (u *uhppote) SetPCControl(deviceID uint32, enable bool) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.SetPCControlRequest{
		SerialNumber: types.SerialNumber(deviceID),
		MagicWord:    0x55aaaa55,
		Enable:       enable,
	}

	response := messages.SetPCControlResponse{}

	err := u.send(deviceID, request, &response)
	if err != nil {
		return false, err
	}

	if uint32(response.SerialNumber) != deviceID {
		return false, fmt.Errorf("incorrect device ID in response - expected '%v', received '%v'", deviceID, response.SerialNumber)
	}

	return response.Succeeded, nil
}
