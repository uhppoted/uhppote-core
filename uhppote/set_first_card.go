package uhppote

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) SetFirstCard(deviceID uint32, firstcard types.FirstCard) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("Invalid device ID (%v)", deviceID)
	}

	request := messages.SetFirstCardRequest{
		SerialNumber:     types.SerialNumber(deviceID),
		Door:             firstcard.Door,
		Start:            firstcard.Start,
		StartDoorControl: uint8(firstcard.StartMode),
		End:              firstcard.End,
		EndDoorControl:   uint8(firstcard.EndMode),
		Monday:           firstcard.Weekdays[time.Monday],
		Tuesday:          firstcard.Weekdays[time.Tuesday],
		Wednesday:        firstcard.Weekdays[time.Wednesday],
		Thursday:         firstcard.Weekdays[time.Thursday],
		Friday:           firstcard.Weekdays[time.Friday],
		Saturday:         firstcard.Weekdays[time.Saturday],
		Sunday:           firstcard.Weekdays[time.Sunday],
	}

	response := messages.SetFirstCardResponse{}

	err := u.send(deviceID, request, &response)
	if err != nil {
		return false, err
	}

	if uint32(response.SerialNumber) != deviceID {
		return false, fmt.Errorf("Incorrect device ID in response - expected '%v', received '%v'", deviceID, response.SerialNumber)
	}

	return response.Succeeded, nil
}
