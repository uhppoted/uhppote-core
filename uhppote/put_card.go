package uhppote

import (
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) PutCard(deviceID uint32, card types.Card) (bool, error) {
	request := messages.PutCardRequest{
		SerialNumber: types.SerialNumber(deviceID),
		CardNumber:   card.CardNumber,
		From:         *card.From,
		To:           *card.To,
		Door1:        card.Doors[1].(bool),
		Door2:        card.Doors[2].(bool),
		Door3:        card.Doors[3].(bool),
		Door4:        card.Doors[4].(bool),
	}

	reply := messages.PutCardResponse{}

	err := u.Send(deviceID, request, &reply)
	if err != nil {
		return false, err
	}

	if uint32(reply.SerialNumber) != deviceID {
		return false, fmt.Errorf("Incorrect serial number in response - expect '%v', received '%v'", deviceID, reply.SerialNumber)
	}

	return reply.Succeeded, nil
}
