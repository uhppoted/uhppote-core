package uhppote

import (
	"fmt"
	"strconv"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) PutCard(deviceID uint32, card types.Card, formats ...types.CardFormat) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	if !isCardNumberValid(card.CardNumber, formats...) {
		return false, fmt.Errorf("invalid card number (%v)", card.CardNumber)
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

	if reply, err := sendto[messages.PutCardResponse](u, deviceID, request); err != nil {
		return false, err
	} else {
		return reply.Succeeded, nil
	}
}

func isCardNumberValid(cardNumber uint32, formats ...types.CardFormat) bool {
	if len(formats) == 0 {
		return true
	}

	for _, f := range formats {
		switch f {
		case types.Wiegand26:
			if isWiegand26(cardNumber) {
				return true
			}

		case types.WiegandAny:
			if isWiegandAny(cardNumber) {
				return true
			}
		}
	}

	return false
}

func isWiegand26(card uint32) bool {
	s := fmt.Sprintf("%08v", card)

	if facilityCode, err := strconv.Atoi(s[:3]); err != nil {
		return false
	} else if cardNumber, err := strconv.Atoi(s[3:]); err != nil {
		return false
	} else if facilityCode < 0 || facilityCode > 255 {
		return false
	} else if cardNumber < 0 || cardNumber > 65535 {
		return false
	}

	return true
}

func isWiegandAny(card uint32) bool {
	return true
}
