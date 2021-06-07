package uhppote

import (
	"errors"
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) DeleteCard(deviceID uint32, cardNumber uint32) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("Invalid device ID (%v)", deviceID)
	}

	request := messages.DeleteCardRequest{
		SerialNumber: types.SerialNumber(deviceID),
		CardNumber:   cardNumber,
	}

	reply := messages.DeleteCardResponse{}

	err := u.send(deviceID, request, &reply)
	if err != nil {
		return false, err
	}

	if uint32(reply.SerialNumber) != deviceID {
		return false, errors.New(fmt.Sprintf("Incorrect serial number in response - expected '%v', received '%v'", deviceID, reply.SerialNumber))
	}

	return reply.Succeeded, nil
}
