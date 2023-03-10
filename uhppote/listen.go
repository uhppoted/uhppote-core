package uhppote

import (
	"os"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

type event messages.GetStatusResponse

type Listener interface {
	OnConnected()
	OnEvent(*types.Status)
	OnError(error) bool
}

func (u *uhppote) Listen(listener Listener, q chan os.Signal) error {
	sysdatetime := func(e *event) types.DateTime {
		if e.SystemDate.IsZero() {
			return types.DateTime{}
		}

		d := e.SystemDate.Format("2006-01-02")
		t := e.SystemTime.Format("15:04:05")

		if dt, err := time.ParseInLocation("2006-01-02 15:04:05", d+" "+t, time.Local); err != nil {
			return types.DateTime{}
		} else {
			return types.DateTime(dt)
		}
	}

	pipe := make(chan *event)

	defer close(pipe)

	go func() {
		for {
			if e := <-pipe; e == nil {
				break
			} else {
				status := types.Status{
					SerialNumber:   e.SerialNumber,
					DoorState:      map[uint8]bool{1: e.Door1State, 2: e.Door2State, 3: e.Door3State, 4: e.Door4State},
					DoorButton:     map[uint8]bool{1: e.Door1Button, 2: e.Door2Button, 3: e.Door3Button, 4: e.Door4Button},
					SystemError:    e.SystemError,
					SystemDateTime: sysdatetime(e),
					SequenceId:     e.SequenceId,
					SpecialInfo:    e.SpecialInfo,
					RelayState:     e.RelayState,
					InputState:     e.InputState,
				}

				if e.EventIndex != 0 {
					status.Event = &types.StatusEvent{
						Index:      e.EventIndex,
						Type:       e.EventType,
						Granted:    e.Granted,
						Door:       e.Door,
						Direction:  e.Direction,
						CardNumber: e.CardNumber,
						Timestamp:  e.Timestamp,
						Reason:     e.Reason,
					}
				}

				listener.OnEvent(&status)
			}
		}
	}()

	return u.listen(pipe, q, listener)
}
