package uhppote

import (
	"fmt"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) DeleteCard(deviceID uint32, cardNumber uint32) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.DeleteCardRequest{
		SerialNumber: types.SerialNumber(deviceID),
		CardNumber:   cardNumber,
	}

	if reply, err := sendto[messages.DeleteCardResponse](u, deviceID, request); err != nil {
		return false, err
	} else {
		return reply.Succeeded, nil
	}
}
