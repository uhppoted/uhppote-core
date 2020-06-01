package uhppote

import (
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) GetCardByIndex(serialNumber, index uint32) (*types.Card, error) {
	request := messages.GetCardByIndexRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		Index:        index,
	}

	reply, err := u.Send(serialNumber, request)
	if err != nil {
		return nil, err
	}

	response, ok := reply.(*messages.GetCardByIndexResponse)
	if !ok {
		return nil, fmt.Errorf("Invalid response to GetCardByIndex")
	}

	if uint32(response.SerialNumber) != serialNumber {
		return nil, fmt.Errorf("Incorrect serial number in response - expect '%v', received '%v'", serialNumber, response.SerialNumber)
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
		Doors:      []bool{response.Door1, response.Door2, response.Door3, response.Door4},
	}, nil
}

func (u *UHPPOTE) GetCardByIndexN(deviceID, index uint32) (*types.Card, error) {
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
		Doors:      []bool{response.Door1, response.Door2, response.Door3, response.Door4},
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
		Doors:      []bool{response.Door1, response.Door2, response.Door3, response.Door4},
	}, nil
}

func (u *UHPPOTE) GetCardByIdN(deviceID, cardID uint32) (*types.Card, error) {
	request := messages.GetCardByIDRequest{
		SerialNumber: types.SerialNumber(deviceID),
		CardNumber:   cardID,
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

	if response.CardNumber != cardID {
		return nil, fmt.Errorf("Incorrect card number in response - expected '%v', received '%v'", cardID, response.CardNumber)
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
		Doors:      []bool{response.Door1, response.Door2, response.Door3, response.Door4},
	}, nil
}
