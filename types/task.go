package types

import (
	"encoding/json"
	"fmt"
	"strings"
)

type Task struct {
	From      *Date    `json:"start-date,omitempty"`
	To        *Date    `json:"end-date,omitempty"`
	Weekdays  Weekdays `json:"weekdays,omitempty"`
	Start     HHmm     `json:"start,omitempty"`
	Door      uint8    `json:"door,omitempty"`
	Task      TaskType `json:"task"`
	MoreCards uint8    `json:"more-cards,omitempty"`
}

type TaskType int

const (
	DoorControlled TaskType = iota
	DoorOpen
	DoorClosed
	DisableTimeProfile
	EnableTimeProfile
	CardNoPassword
	CardInPassword
	CardInOutPassword
	EnableMoreCards
	DisableMoreCards
	TriggerOnce
	DisablePushButton
	EnablePushButton
)

func (tt TaskType) String() string {
	return [...]string{
		"DOOR CONTROLLED",
		"DOOR OPEN",
		"DOOR CLOSED",
		"DISABLE TIME PROFILE",
		"ENABLE TIME PROFILE",
		"CARD - NO PASSWORD",
		"CARD - IN PASSWORD",
		"CARD - IN/OUT PASSWORD",
		"ENABLE MORE CARDS",
		"DISABLE MORE CARDS",
		"TRIGGER ONCE",
		"DISABLE PUSH BUTTON",
		"ENABLE PUSH BUTTON",
	}[tt]
}

func (t Task) String() string {
	from := fmt.Sprintf("%v", t.From)
	to := fmt.Sprintf("%v", t.To)
	dates := ""

	if from != "" && to != "" {
		dates = from + ":" + to
	} else if from != "" {
		dates = from + ":-"
	} else {
		dates = "-:" + to
	}

	weekdays := fmt.Sprintf("%v", t.Weekdays)
	start := fmt.Sprintf("%v", t.Start)
	door := fmt.Sprintf("%v", t.Door)
	task := fmt.Sprintf("%v", t.Task)
	cards := fmt.Sprintf("%v", t.MoreCards)

	list := []string{}
	for _, s := range []string{task, dates, weekdays, start, door, cards} {
		if s != "" {
			list = append(list, s)
		}
	}

	return strings.Join(list, " ")
}

func (t *Task) UnmarshalJSON(bytes []byte) error {
	task := struct {
		From      *Date    `json:"start-date,omitempty"`
		To        *Date    `json:"end-date,omitempty"`
		Weekdays  Weekdays `json:"weekdays,omitempty"`
		Start     HHmm     `json:"start,omitempty"`
		Door      uint8    `json:"door,omitempty"`
		Task      TaskType `json:"task"`
		MoreCards uint8    `json:"more-cards,omitempty"`
	}{
		Weekdays: Weekdays{},
	}

	if err := json.Unmarshal(bytes, &task); err != nil {
		return err
	}

	t.From = task.From
	t.To = task.To
	t.Weekdays = task.Weekdays
	t.Start = task.Start
	t.Door = task.Door
	t.Task = task.Task
	t.MoreCards = task.MoreCards

	return nil
}
