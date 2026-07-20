package uhppote

import (
	"fmt"
	"time"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) SetTime(serialNumber uint32, datetime time.Time) (*types.Time, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.SetTimeRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		DateTime:     types.DateTime(datetime),
	}

	if reply, err := sendto[messages.SetTimeResponse](u, serialNumber, request); err != nil {
		return nil, err
	} else {
		return &types.Time{
			SerialNumber: reply.SerialNumber,
			DateTime:     reply.DateTime,
		}, nil
	}
}
