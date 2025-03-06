package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetAntiPassback(serialNumber uint32) (types.AntiPassback, error) {
	if serialNumber == 0 {
		return 0, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.GetAntiPassbackRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	if reply, err := sendto[messages.GetAntiPassbackResponse](u, serialNumber, request); err != nil {
		return 0, err
	} else {
		return types.AntiPassback(reply.Mode), nil
	}
}
