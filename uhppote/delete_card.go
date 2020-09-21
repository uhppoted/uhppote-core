package uhppote

import (
	"errors"
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) DeleteCard(serialNumber, cardNumber uint32) (*types.Result, error) {
	request := messages.DeleteCardRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		CardNumber:   cardNumber,
	}

	reply := messages.DeleteCardResponse{}

	err := u.Execute(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	if uint32(reply.SerialNumber) != serialNumber {
		return nil, errors.New(fmt.Sprintf("Incorrect serial number in response - expected '%v', received '%v'", serialNumber, reply.SerialNumber))
	}

	return &types.Result{
		SerialNumber: reply.SerialNumber,
		Succeeded:    reply.Succeeded,
	}, nil
}

func (u *UHPPOTE) DeleteCardN(deviceID uint32, card types.CardX) (bool, error) {
	request := messages.DeleteCardRequest{
		SerialNumber: types.SerialNumber(deviceID),
		CardNumber:   card.CardNumber,
	}

	reply := messages.DeleteCardResponse{}

	err := u.Execute(deviceID, request, &reply)
	if err != nil {
		return false, err
	}

	if uint32(reply.SerialNumber) != deviceID {
		return false, errors.New(fmt.Sprintf("Incorrect serial number in response - expected '%v', received '%v'", deviceID, reply.SerialNumber))
	}

	return reply.Succeeded, nil
}
