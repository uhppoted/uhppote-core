package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
	"time"
)

func (u *uhppote) SetTime(serialNumber uint32, datetime time.Time) (*types.Time, error) {
	request := messages.SetTimeRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		DateTime:     types.DateTime(datetime),
	}

	reply := messages.SetTimeResponse{}

	err := u.send(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	return &types.Time{
		SerialNumber: reply.SerialNumber,
		DateTime:     reply.DateTime,
	}, nil
}
