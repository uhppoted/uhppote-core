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

	if reply, err := u.sendTo(serialNumber, request, messages.GetStatusResponse{}); err != nil {
		return nil, err
	} else {
		response := reply.(messages.GetStatusResponse)

		sysdatetime := func() types.DateTime {
			if response.SystemDate.IsZero() {
				return types.DateTime{}
			}

			d := response.SystemDate.Format("2006-01-02")
			t := response.SystemTime.Format("15:04:05")

			if dt, err := time.ParseInLocation("2006-01-02 15:04:05", d+" "+t, time.Local); err != nil {
				return types.DateTime{}
			} else {
				return types.DateTime(dt)
			}
		}

		status := types.Status{
			SerialNumber:   response.SerialNumber,
			DoorState:      map[uint8]bool{1: response.Door1State, 2: response.Door2State, 3: response.Door3State, 4: response.Door4State},
			DoorButton:     map[uint8]bool{1: response.Door1Button, 2: response.Door2Button, 3: response.Door3Button, 4: response.Door4Button},
			SystemError:    response.SystemError,
			SystemDateTime: sysdatetime(),
			SequenceId:     response.SequenceId,
			SpecialInfo:    response.SpecialInfo,
			RelayState:     response.RelayState,
			InputState:     response.InputState,
		}

		if response.EventIndex != 0 {
			status.Event = types.StatusEvent{
				Index:      response.EventIndex,
				Type:       response.EventType,
				Granted:    response.Granted,
				Door:       response.Door,
				Direction:  response.Direction,
				CardNumber: response.CardNumber,
				Timestamp:  response.Timestamp,
				Reason:     response.Reason,
			}
		}

		return &status, nil
	}
}
