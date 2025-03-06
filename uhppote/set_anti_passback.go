package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) SetAntiPassback(serialNumber uint32, mode types.AntiPassback) (bool, error) {
	if serialNumber == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.SetAntiPassbackRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		Mode:         uint8(mode),
	}

	if reply, err := sendto[messages.SetAntiPassbackResponse](u, serialNumber, request); err != nil {
		return false, err
	} else {
		return reply.Ok, nil
	}
}
