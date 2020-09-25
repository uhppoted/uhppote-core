package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
	"os"
	"time"
)

type event messages.GetStatusResponse

type Listener interface {
	OnConnected()
	OnEvent(*types.Status)
	OnError(error) bool
}

func (u *UHPPOTE) Listen(listener Listener, q chan os.Signal) error {
	pipe := make(chan *event)

	defer close(pipe)

	go func() {
		for {
			if e := <-pipe; e == nil {
				break
			} else {
				d := time.Time(e.SystemDate).Format("2006-01-02")
				t := time.Time(e.SystemTime).Format("15:04:05")
				datetime, _ := time.ParseInLocation("2006-01-02 15:04:05", d+" "+t, time.Local)

				status := &types.Status{
					SerialNumber:   e.SerialNumber,
					EventIndex:     e.EventIndex,
					EventType:      e.EventType,
					Granted:        e.Granted,
					Door:           e.Door,
					Direction:      e.Direction,
					CardNumber:     e.CardNumber,
					Timestamp:      e.Timestamp,
					Reason:         e.Reason,
					DoorState:      map[uint8]bool{1: e.Door1State, 2: e.Door2State, 3: e.Door3State, 4: e.Door4State},
					DoorButton:     map[uint8]bool{1: e.Door1Button, 2: e.Door2Button, 3: e.Door3Button, 4: e.Door4Button},
					SystemError:    e.SystemError,
					SystemDateTime: types.DateTime(datetime),
					SequenceId:     e.SequenceId,
					SpecialInfo:    e.SpecialInfo,
					RelayState:     e.RelayState,
					InputState:     e.InputState,
				}
				listener.OnEvent(status)
			}
		}
	}()

	return u.listen(pipe, q, listener)
}
