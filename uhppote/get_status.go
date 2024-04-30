package uhppote

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetStatus(serialNumber uint32) (*types.Status, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.GetStatusRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	if reply, err := sendto[messages.GetStatusResponse](u, serialNumber, request); err != nil {
		return nil, err
	} else {
		sysdatetime := func() types.DateTime {
			if reply.SystemDate.IsZero() {
				return types.DateTime{}
			}

			d := reply.SystemDate.Format("2006-01-02")
			t := reply.SystemTime.Format("15:04:05")

			if dt, err := time.ParseInLocation("2006-01-02 15:04:05", d+" "+t, time.Local); err != nil {
				return types.DateTime{}
			} else {
				return types.DateTime(dt)
			}
		}

		status := types.Status{
			SerialNumber:   reply.SerialNumber,
			DoorState:      map[uint8]bool{1: reply.Door1State, 2: reply.Door2State, 3: reply.Door3State, 4: reply.Door4State},
			DoorButton:     map[uint8]bool{1: reply.Door1Button, 2: reply.Door2Button, 3: reply.Door3Button, 4: reply.Door4Button},
			SystemError:    reply.SystemError,
			SystemDateTime: sysdatetime(),
			SequenceId:     reply.SequenceId,
			SpecialInfo:    reply.SpecialInfo,
			RelayState:     reply.RelayState,
			InputState:     reply.InputState,
		}

		if reply.EventIndex != 0 {
			status.Event = types.StatusEvent{
				Index:      reply.EventIndex,
				Type:       reply.EventType,
				Granted:    reply.Granted,
				Door:       reply.Door,
				Direction:  reply.Direction,
				CardNumber: reply.CardNumber,
				Timestamp:  reply.Timestamp,
				Reason:     reply.Reason,
			}
		}

		return &status, nil
	}
}
