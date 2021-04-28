package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) GetTime(serialNumber uint32) (*types.Time, error) {
	request := messages.GetTimeRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	reply := messages.GetTimeResponse{}

	err := u.Send(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	return &types.Time{
		SerialNumber: reply.SerialNumber,
		DateTime:     reply.DateTime,
	}, nil
}
