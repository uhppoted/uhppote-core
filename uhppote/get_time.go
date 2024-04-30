package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetTime(serialNumber uint32) (*types.Time, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.GetTimeRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	if reply, err := u.sendTo(serialNumber, request, messages.GetTimeResponse{}); err != nil {
		return nil, err
	} else {
		response := reply.(messages.GetTimeResponse)

		return &types.Time{
			SerialNumber: response.SerialNumber,
			DateTime:     response.DateTime,
		}, nil
	}
}
