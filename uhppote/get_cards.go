package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) GetCards(deviceID uint32) (uint32, error) {
	request := messages.GetCardsRequest{
		SerialNumber: types.SerialNumber(deviceID),
	}

	reply := messages.GetCardsResponse{}

	err := u.Execute(deviceID, request, &reply)
	if err != nil {
		return 0, err
	}

	return reply.Records, nil
}
