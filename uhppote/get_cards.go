package uhppote

import (
	"fmt"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) GetCards(deviceID uint32) (uint32, error) {
	if deviceID == 0 {
		return 0, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.GetCardsRequest{
		SerialNumber: types.SerialNumber(deviceID),
	}

	if reply, err := sendto[messages.GetCardsResponse](u, deviceID, request); err != nil {
		return 0, err
	} else {
		return reply.Records, nil
	}
}
