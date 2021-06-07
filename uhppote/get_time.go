package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetTime(serialNumber uint32) (*types.Time, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("Invalid device ID (%v)", serialNumber)
	}

	request := messages.GetTimeRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	reply := messages.GetTimeResponse{}

	err := u.send(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	return &types.Time{
		SerialNumber: reply.SerialNumber,
		DateTime:     reply.DateTime,
	}, nil
}
