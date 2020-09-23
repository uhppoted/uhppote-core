package uhppote

import (
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) GetCardByIndex(deviceID, index uint32) (*types.Card, error) {
	return GetCardByIndex(u, deviceID, index)
}

func GetCardByIndex(u iuhppote, deviceID, index uint32) (*types.Card, error) {
	request := messages.GetCardByIndexRequest{
		SerialNumber: types.SerialNumber(deviceID),
		Index:        index,
	}

	reply, err := u.Send(deviceID, request)
	if err != nil {
		return nil, err
	}

	response, ok := reply.(*messages.GetCardByIndexResponse)
	if !ok {
		return nil, fmt.Errorf("Invalid response to GetCardByIndex")
	}

	if uint32(response.SerialNumber) != deviceID {
		return nil, fmt.Errorf("Incorrect device ID in response - expected '%v', received '%v'", deviceID, response.SerialNumber)
	}

	// 0:          not found
	// 0xffffffff: deleted
	if response.CardNumber == 0 || response.CardNumber == 0xffffffff {
		return nil, nil
	}

	if response.From == nil {
		return nil, fmt.Errorf("Invalid 'from' date in response")
	}

	if response.To == nil {
		return nil, fmt.Errorf("Invalid 'to' date in response")
	}

	return &types.Card{
		CardNumber: response.CardNumber,
		From:       response.From,
		To:         response.To,
		Doors:      map[uint8]bool{1: response.Door1, 2: response.Door2, 3: response.Door3, 4: response.Door4},
	}, nil
}

func (u *UHPPOTE) GetCardByID(serialNumber, cardNumber uint32) (*types.Card, error) {
	request := messages.GetCardByIDRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		CardNumber:   cardNumber,
	}

	reply, err := u.Send(serialNumber, request)
	if err != nil {
		return nil, err
	}

	response, ok := reply.(*messages.GetCardByIDResponse)
	if !ok {
		return nil, fmt.Errorf("Invalid response to GetCardById")
	}

	if uint32(response.SerialNumber) != serialNumber {
		return nil, fmt.Errorf("Incorrect serial number in response - expected '%v', received '%v'", serialNumber, response.SerialNumber)
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

	return &types.Card{
		CardNumber: response.CardNumber,
		From:       response.From,
		To:         response.To,
		Doors:      map[uint8]bool{1: response.Door1, 2: response.Door2, 3: response.Door3, 4: response.Door4},
	}, nil
}

func (u *UHPPOTE) GetCardByIdN(deviceID, cardNumber uint32) (*types.Card, error) {
	request := messages.GetCardByIDRequest{
		SerialNumber: types.SerialNumber(deviceID),
		CardNumber:   cardNumber,
	}

	reply, err := u.Send(deviceID, request)
	if err != nil {
		return nil, err
	}

	response, ok := reply.(*messages.GetCardByIDResponse)
	if !ok {
		return nil, fmt.Errorf("Invalid response to GetCardById")
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

	return &types.Card{
		CardNumber: response.CardNumber,
		From:       response.From,
		To:         response.To,
		Doors:      map[uint8]bool{1: response.Door1, 2: response.Door2, 3: response.Door3, 4: response.Door4},
	}, nil
}
