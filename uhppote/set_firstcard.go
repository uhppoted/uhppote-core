package uhppote

import (
	"fmt"
	"time"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) SetFirstCard(serialNumber uint32, door uint8, firstcard types.FirstCard) (bool, error) {
	if serialNumber == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	active := uint8(0)
	switch firstcard.Active {
	case types.ModeControlled:
		active = uint8(0)
	case types.ModeNormallyOpen:
		active = uint8(1)
	case types.ModeNormallyClosed:
		active = uint8(2)
	default:
		if firstcard.Active == types.ModeUnknown {
			return false, fmt.Errorf("invalid first-card active mode (unknown)")
		} else {
			return false, fmt.Errorf("invalid first-card active mode (%v)", firstcard.Active)
		}
	}

	inactive := uint8(0)
	switch firstcard.Inactive {
	case types.ModeControlled:
		inactive = uint8(0)
	case types.ModeNormallyOpen:
		inactive = uint8(1)
	case types.ModeNormallyClosed:
		inactive = uint8(2)
	case types.ModeFirstCardOnly:
		inactive = uint8(3)
	default:
		if firstcard.Inactive == types.ModeUnknown {
			return false, fmt.Errorf("invalid first-card inactive mode (unknown)")
		} else {
			return false, fmt.Errorf("invalid first-card inactive mode (%v)", firstcard.Inactive)
		}
	}

	request := messages.SetFirstCardRequest{
		SerialNumber:     types.SerialNumber(serialNumber),
		Door:             door,
		StartTime:        firstcard.StartTime,
		EndTime:          firstcard.EndTime,
		StartDoorControl: active,
		EndDoorControl:   inactive,
		Monday:           firstcard.Weekdays[time.Monday],
		Tuesday:          firstcard.Weekdays[time.Tuesday],
		Wednesday:        firstcard.Weekdays[time.Wednesday],
		Thursday:         firstcard.Weekdays[time.Thursday],
		Friday:           firstcard.Weekdays[time.Friday],
		Saturday:         firstcard.Weekdays[time.Saturday],
		Sunday:           firstcard.Weekdays[time.Sunday],
	}

	if reply, err := sendto[messages.SetFirstCardResponse](u, serialNumber, request); err != nil {
		return false, err
	} else {
		return reply.Ok, nil
	}
}
