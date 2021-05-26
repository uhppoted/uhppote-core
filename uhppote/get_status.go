package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
	"time"
)

func (u *uhppote) GetStatus(serialNumber uint32) (*types.Status, error) {
	request := messages.GetStatusRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	reply := messages.GetStatusResponse{}

	err := u.send(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	d := time.Time(reply.SystemDate).Format("2006-01-02")
	t := time.Time(reply.SystemTime).Format("15:04:05")
	datetime, _ := time.ParseInLocation("2006-01-02 15:04:05", d+" "+t, time.Local)

	status := types.Status{
		SerialNumber:   reply.SerialNumber,
		DoorState:      map[uint8]bool{1: reply.Door1State, 2: reply.Door2State, 3: reply.Door3State, 4: reply.Door4State},
		DoorButton:     map[uint8]bool{1: reply.Door1Button, 2: reply.Door2Button, 3: reply.Door3Button, 4: reply.Door4Button},
		SystemError:    reply.SystemError,
		SystemDateTime: types.DateTime(datetime),
		SequenceId:     reply.SequenceId,
		SpecialInfo:    reply.SpecialInfo,
		RelayState:     reply.RelayState,
		InputState:     reply.InputState,
	}

	if reply.EventIndex != 0 {
		status.Event = &types.StatusEvent{
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
