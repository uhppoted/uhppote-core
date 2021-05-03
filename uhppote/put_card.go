package uhppote

import (
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) PutCard(deviceID uint32, card types.Card) (bool, error) {
	f := func(p types.Permission) uint8 {
		if p != nil {
			switch v := p.(type) {
			case bool:
				if v {
					return 1
				}

			case int:
				if v > 1 && v < 255 {
					return uint8(v)
				}

			case uint:
				if v > 1 && v < 255 {
					return uint8(v)
				}
			}
		}

		return 0
	}

	request := messages.PutCardRequest{
		SerialNumber: types.SerialNumber(deviceID),
		CardNumber:   card.CardNumber,
		From:         *card.From,
		To:           *card.To,
		Door1:        f(card.Doors[1]),
		Door2:        f(card.Doors[2]),
		Door3:        f(card.Doors[3]),
		Door4:        f(card.Doors[4]),
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
