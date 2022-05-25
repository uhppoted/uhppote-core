package uhppote

import (
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetCardByID(deviceID, cardNumber uint32) (*types.Card, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("Invalid device ID (%v)", deviceID)
	}

	request := messages.GetCardByIDRequest{
		SerialNumber: types.SerialNumber(deviceID),
		CardNumber:   cardNumber,
	}

	response := messages.GetCardByIDResponse{}

	err := u.send(deviceID, request, &response)
	if err != nil {
		return nil, err
	}

	if uint32(response.SerialNumber) != deviceID {
		return nil, fmt.Errorf("Incorrect serial number in response - expected '%v', received '%v'", deviceID, response.SerialNumber)
	}

	if response.CardNumber == 0 {
		return nil, nil
	}

	if response.CardNumber != cardNumber {
		return nil, fmt.Errorf("Incorrect card number in response - expected '%v', received '%v'", cardNumber, response.CardNumber)
	}

	if response.From == nil {
		return nil, fmt.Errorf("Invalid 'from' date in response")
	}

	if response.To == nil {
		return nil, fmt.Errorf("Invalid 'to' date in response")
	}

	card := types.Card{
		CardNumber: response.CardNumber,
		From:       response.From,
		To:         response.To,
		Doors: map[uint8]uint8{
			1: response.Door1,
			2: response.Door2,
			3: response.Door3,
			4: response.Door4,
		},
	}

	return &card, nil
}

func (u *uhppote) GetCardByIndex(deviceID, index uint32) (*types.Card, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("Invalid device ID (%v)", deviceID)
	}

	request := messages.GetCardByIndexRequest{
		SerialNumber: types.SerialNumber(deviceID),
		Index:        index,
	}

	response := messages.GetCardByIndexResponse{}

	err := u.send(deviceID, request, &response)
	if err != nil {
		return nil, err
	}

	if uint32(response.SerialNumber) != deviceID {
		return nil, fmt.Errorf("Incorrect device ID in response - expected '%v', received '%v'", deviceID, response.SerialNumber)
	}

	notFound := response.CardNumber == 0x0
	deleted := response.CardNumber == 0xffffffff
	if notFound || deleted {
		return nil, nil
	}

	if response.From == nil {
		return nil, fmt.Errorf("Invalid 'from' date in response")
	}

	if response.To == nil {
		return nil, fmt.Errorf("Invalid 'to' date in response")
	}

	card := types.Card{
		CardNumber: response.CardNumber,
		From:       response.From,
		To:         response.To,
		Doors: map[uint8]uint8{
			1: response.Door1,
			2: response.Door2,
			3: response.Door3,
			4: response.Door4,
		},
	}

	return &card, nil
}
