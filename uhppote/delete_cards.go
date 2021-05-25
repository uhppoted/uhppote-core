package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) DeleteCards(serialNumber uint32) (bool, error) {
	request := messages.DeleteCardsRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		MagicWord:    0x55aaaa55,
	}

	reply := messages.DeleteCardsResponse{}

	err := u.Send(serialNumber, request, &reply)
	if err != nil {
		return false, err
	}

	return reply.Succeeded, nil
}
