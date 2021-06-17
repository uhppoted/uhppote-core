package uhppote

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) AddTask(deviceID uint32, task types.Task) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("Invalid device ID (%v)", deviceID)
	}

	request := messages.AddTaskRequest{
		SerialNumber: types.SerialNumber(deviceID),
		From:         task.From,
		To:           task.To,
		Monday:       task.Weekdays[time.Monday],
		Tuesday:      task.Weekdays[time.Tuesday],
		Wednesday:    task.Weekdays[time.Wednesday],
		Thursday:     task.Weekdays[time.Thursday],
		Friday:       task.Weekdays[time.Friday],
		Saturday:     task.Weekdays[time.Saturday],
		Sunday:       task.Weekdays[time.Sunday],
		Start:        task.Start,
		Door:         task.Door,
		Task:         uint8(task.Task),
		MoreCards:    task.Cards,
	}

	response := messages.AddTaskResponse{}

	err := u.send(deviceID, request, &response)
	if err != nil {
		return false, err
	}

	if uint32(response.SerialNumber) != deviceID {
		return false, fmt.Errorf("Incorrect device ID in response - expected '%v', received '%v'", deviceID, response.SerialNumber)
	}

	return response.Succeeded, nil
}
