package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

// Sends a SetSuperPassword request to the designated controller, to set the override
// PIN codes for a door managed by the access controller.
//
// Each door may be individually assigned up to four super password, with valid passwords
// being in the range [1..999999]. The function uses the first four passwords from the
// supplied list. Invalid passwords are disabled by setting the password to 0 (disabled).
// If the supplied list contains less than 4 passwords, the remaining entries on the
// controller will be set to 0 (disabled).
//
// Returns true if the super passwords were accepted by the access controller.
func (u *uhppote) SetSuperPassword(controller uint32, door uint8, passwords []uint32) (bool, error) {
	if controller == 0 {
		return false, fmt.Errorf("invalid controller ID (%v)", controller)
	}

	if door < 1 || door > 4 {
		return false, fmt.Errorf("invalid controller door (%v) - valid range is 1..4", door)
	}

	request := messages.SetSuperPasswordRequest{
		SerialNumber: types.SerialNumber(controller),
		Door:         door,
		Password1:    0,
		Password2:    0,
		Password3:    0,
		Password4:    0,
	}

	if len(passwords) > 0 && passwords[0] <= 999999 {
		request.Password1 = passwords[0]
	}

	if len(passwords) > 1 && passwords[1] <= 999999 {
		request.Password2 = passwords[1]
	}

	if len(passwords) > 2 && passwords[2] <= 999999 {
		request.Password3 = passwords[2]
	}

	if len(passwords) > 3 && passwords[3] <= 999999 {
		request.Password4 = passwords[3]
	}

	response := messages.SetSuperPasswordResponse{}

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
