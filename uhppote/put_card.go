package uhppote

import (
	"fmt"
	"strconv"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) PutCard(deviceID uint32, card types.Card) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	if err := validateCard(card.CardNumber); err != nil {
		return false, err
	}

	if card.PIN > 999999 {
		return false, fmt.Errorf("invalid PIN (%v)", card.PIN)
	}

	request := messages.PutCardRequest{
		SerialNumber: types.SerialNumber(deviceID),
		CardNumber:   card.CardNumber,
		From:         card.From,
		To:           card.To,
		Door1:        uint8(card.Doors[1]),
		Door2:        uint8(card.Doors[2]),
		Door3:        uint8(card.Doors[3]),
		Door4:        uint8(card.Doors[4]),
		PIN:          card.PIN,
	}

	reply := messages.PutCardResponse{}

	err := u.send(deviceID, request, &reply)
	if err != nil {
		return false, err
	}

	if uint32(reply.SerialNumber) != deviceID {
		return false, fmt.Errorf("incorrect serial number in response - expect '%v', received '%v'", deviceID, reply.SerialNumber)
	}

	return reply.Succeeded, nil
}

func validateCard(card uint32) error {
	s := fmt.Sprintf("%08v", card)

	if facilityCode, err := strconv.Atoi(s[:3]); err != nil {
		return fmt.Errorf("invalid card number %v (%v)", card, err)
	} else if cardNumber, err := strconv.Atoi(s[3:]); err != nil {
		return fmt.Errorf("invalid card number %v (%v)", card, err)
	} else if facilityCode < 0 || facilityCode > 255 {
		return fmt.Errorf("%v: invalid facility code %v", card, facilityCode)
	} else if cardNumber < 0 || cardNumber > 65535 {
		return fmt.Errorf("%v: invalid card number %v", card, cardNumber)
	}

	return nil
}
