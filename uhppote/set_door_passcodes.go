package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

// Sends a SetDoorPasscodes request to the designated controller, to set the override
// PIN codes for a door managed by the access controller.
//
// Each door may be individually assigned up to four passcodes, with valid passcodes
// being in the range [1..999999]. The function uses the first four codes from the
// supplied list and invalid passcodes are set to 0 (disabled). If the supplied list
// contains less than four codes, the remaining entries on the controller will be set
// to 0 (disabled).
//
// Returns true if the door passcodes were accepted by the access controller.
func (u *uhppote) SetDoorPasscodes(controller uint32, door uint8, passcodes ...uint32) (bool, error) {
	if controller == 0 {
		return false, fmt.Errorf("invalid controller ID (%v)", controller)
	}

	if door < 1 || door > 4 {
		return false, fmt.Errorf("invalid controller door (%v) - valid range is 1..4", door)
	}

	request := messages.SetDoorPasscodesRequest{
		SerialNumber: types.SerialNumber(controller),
		Door:         door,
		Passcode1:    0,
		Passcode2:    0,
		Passcode3:    0,
		Passcode4:    0,
	}

	if len(passcodes) > 0 && passcodes[0] <= 999999 {
		request.Passcode1 = passcodes[0]
	}

	if len(passcodes) > 1 && passcodes[1] <= 999999 {
		request.Passcode2 = passcodes[1]
	}

	if len(passcodes) > 2 && passcodes[2] <= 999999 {
		request.Passcode3 = passcodes[2]
	}

	if len(passcodes) > 3 && passcodes[3] <= 999999 {
		request.Passcode4 = passcodes[3]
	}

	response := messages.SetDoorPasscodesResponse{}

	err := u.send(controller, request, &response)
	if err != nil {
		return false, err
	}

	if uint32(response.SerialNumber) != controller {
		return false, fmt.Errorf("incorrect controller ID in response - expected '%v', received '%v'",
			controller,
			response.SerialNumber)
	}

	return response.Succeeded, nil
}
