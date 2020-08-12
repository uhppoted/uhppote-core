package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
	"time"
)

func (u *UHPPOTE) GetStatus(serialNumber uint32) (*types.Status, error) {
	request := messages.GetStatusRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	reply := messages.GetStatusResponse{}

	err := u.Execute(serialNumber, request, &reply)
	if err != nil {
		return nil, err
	}

	d := time.Time(reply.SystemDate).Format("2006-01-02")
	t := time.Time(reply.SystemTime).Format("15:04:05")
	datetime, _ := time.ParseInLocation("2006-01-02 15:04:05", d+" "+t, time.Local)

	return &types.Status{
		SerialNumber:   reply.SerialNumber,
		EventIndex:     reply.EventIndex,
		EventType:      reply.EventType,
		Granted:        reply.Granted,
		Door:           reply.Door,
		Direction:      reply.Direction,
		CardNumber:     reply.CardNumber,
		Timestamp:      reply.Timestamp,
		Reason:         reply.Reason,
		DoorState:      []bool{reply.Door1State, reply.Door2State, reply.Door3State, reply.Door4State},
		DoorButton:     []bool{reply.Door1Button, reply.Door2Button, reply.Door3Button, reply.Door4Button},
		SystemError:    reply.SystemError,
		SystemDateTime: types.DateTime(datetime),
		SequenceId:     reply.SequenceId,
		SpecialInfo:    reply.SpecialInfo,
		RelayState:     reply.RelayState,
		InputState:     reply.InputState,
	}, nil
}
