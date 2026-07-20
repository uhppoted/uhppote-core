package uhppote

import (
	"fmt"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) GetTime(serialNumber uint32) (*types.Time, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.GetTimeRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	if reply, err := sendto[messages.GetTimeResponse](u, serialNumber, request); err != nil {
		return nil, err
	} else {
		return &types.Time{
			SerialNumber: reply.SerialNumber,
			DateTime:     reply.DateTime,
		}, nil
	}
}
