package uhppote

import (
	"fmt"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) SetAntiPassback(serialNumber uint32, antipassback types.AntiPassback) (bool, error) {
	if serialNumber == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.SetAntiPassbackRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		AntiPassback: uint8(antipassback),
	}

	if reply, err := sendto[messages.SetAntiPassbackResponse](u, serialNumber, request); err != nil {
		return false, err
	} else {
		return reply.Ok, nil
	}
}
