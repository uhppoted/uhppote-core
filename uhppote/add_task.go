package uhppote

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) AddTask(deviceID uint32, task types.Task) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", deviceID)
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

	if reply, err := u.sendTo(deviceID, request, messages.AddTaskResponse{}); err != nil {
		return false, err
	} else {
		return reply.(messages.AddTaskResponse).Succeeded, nil
	}
}
