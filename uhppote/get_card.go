package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetCardByID(deviceID, cardNumber uint32) (*types.Card, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.GetCardByIDRequest{
		SerialNumber: types.SerialNumber(deviceID),
		CardNumber:   cardNumber,
	}

	if reply, err := sendto[messages.GetCardByIDResponse](u, deviceID, request); err != nil {
		return nil, err
	} else {
		if reply.CardNumber == 0 {
			return nil, nil
		}

		if reply.CardNumber != cardNumber {
			return nil, fmt.Errorf("incorrect card number in response - expected '%v', received '%v'", cardNumber, reply.CardNumber)
		}

		// if reply.From.IsZero() {
		// 	return nil, fmt.Errorf("invalid 'from' date in response")
		// }

		// if reply.To.IsZero() {
		// 	return nil, fmt.Errorf("invalid 'to' date in response")
		// }

		card := types.Card{
			CardNumber: reply.CardNumber,
			From:       reply.From,
			To:         reply.To,
			Doors: map[uint8]uint8{
				1: reply.Door1,
				2: reply.Door2,
				3: reply.Door3,
				4: reply.Door4,
			},
			PIN: reply.PIN,
		}

		return &card, nil
	}
}

func (u *uhppote) GetCardByIndex(deviceID, index uint32) (*types.Card, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.GetCardByIndexRequest{
		SerialNumber: types.SerialNumber(deviceID),
		Index:        index,
	}

	if reply, err := sendto[messages.GetCardByIndexResponse](u, deviceID, request); err != nil {
		return nil, err
	} else {
		notFound := reply.CardNumber == 0x0
		deleted := reply.CardNumber == 0xffffffff
		if notFound || deleted {
			return nil, nil
		}

		// if reply.From.IsZero() {
		// 	return nil, fmt.Errorf("invalid 'from' date in response")
		// }

		// if reply.To.IsZero() {
		// 	return nil, fmt.Errorf("invalid 'to' date in response")
		// }

		card := types.Card{
			CardNumber: reply.CardNumber,
			From:       reply.From,
			To:         reply.To,
			Doors: map[uint8]uint8{
				1: reply.Door1,
				2: reply.Door2,
				3: reply.Door3,
				4: reply.Door4,
			},
			PIN: reply.PIN,
		}

		return &card, nil
	}
}
