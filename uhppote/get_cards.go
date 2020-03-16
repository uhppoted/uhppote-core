package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) GetCards(serialNumber uint32) (*types.RecordCount, error) {
	request := messages.GetCardsRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	reply := messages.GetCardsResponse{}

	err := u.Execute(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	return &types.RecordCount{
		SerialNumber: reply.SerialNumber,
		Records:      reply.Records,
	}, nil
}

func (u *UHPPOTE) GetCardsN(deviceID uint32) (uint32, error) {
	request := messages.GetCardsRequest{
		SerialNumber: types.SerialNumber(deviceID),
	}

	reply := messages.GetCardsResponse{}

	err := u.Execute(deviceID, request, &reply)
	if err != nil {
		return 0, err
	}

	return reply.Records,nil
}
