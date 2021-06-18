package types

import (
	"encoding/json"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

type Task struct {
	Task     TaskType `json:"task"`
	Door     uint8    `json:"door,omitempty"`
	From     Date     `json:"start-date,omitempty"`
	To       Date     `json:"end-date,omitempty"`
	Weekdays Weekdays `json:"weekdays,omitempty"`
	Start    HHmm     `json:"start,omitempty"`
	Cards    uint8    `json:"cards,omitempty"`
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
		"CONTROL DOOR",
		"UNLOCK DOOR",
		"LOCK DOOR",
		"DISABLE TIME PROFILE",
		"ENABLE TIME PROFILE",
		"ENABLE CARD, NO PASSWORD",
		"ENABLE CARD+IN PASSWORD",
		"ENABLE CARD+PASSWORD",
		"ENABLE MORE CARDS",
		"DISABLE MORE CARDS",
		"TRIGGER ONCE",
		"DISABLE PUSH BUTTON",
		"ENABLE PUSH BUTTON",
	}[tt]
}

func (tt *TaskType) UnmarshalTSV(s string) (interface{}, error) {
	// ... numeric task type?
	if regexp.MustCompile("^[0-9]+$").MatchString(s) {
		if v, err := strconv.Atoi(s); err != nil {
			return nil, err
		} else if v < 1 || v > 13 {
			return nil, fmt.Errorf("invalid task type code (%v)", v)
		} else {
			return TaskType(v), nil
		}
	}

	// ... text task type
	re := regexp.MustCompile("[^a-z]+")
	clean := func(s string) string { return re.ReplaceAllString(strings.ToLower(s), "") }
	task := clean(s)

	for _, v := range []TaskType{
		DoorControlled,
		DoorOpen,
		DoorClosed,
		DisableTimeProfile,
		EnableTimeProfile,
		CardNoPassword,
		CardInPassword,
		CardInOutPassword,
		EnableMoreCards,
		DisableMoreCards,
		TriggerOnce,
		DisablePushButton,
		EnablePushButton,
	} {
		if task == clean(fmt.Sprintf("%v", v)) {
			return TaskType(v), nil
		}
	}

	return nil, fmt.Errorf("invalid task type (%v)", s)
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
	task := fmt.Sprintf("%-24v", t.Task)
	cards := ""

	if t.Task == EnableMoreCards {
		cards = fmt.Sprintf("%v", t.Cards)
	}

	list := []string{}
	for _, s := range []string{task, door, dates, weekdays, start, cards} {
		if s != "" {
			list = append(list, s)
		}
	}

	return strings.Join(list, " ")
}

func (t *Task) UnmarshalJSON(bytes []byte) error {
	task := struct {
		From     *Date    `json:"start-date,omitempty"`
		To       *Date    `json:"end-date,omitempty"`
		Weekdays Weekdays `json:"weekdays,omitempty"`
		Start    HHmm     `json:"start,omitempty"`
		Door     uint8    `json:"door,omitempty"`
		Task     TaskType `json:"task"`
		Cards    uint8    `json:"cards,omitempty"`
	}{
		Weekdays: Weekdays{},
	}

	if err := json.Unmarshal(bytes, &task); err != nil {
		return err
	}

	if task.From == nil {
		return fmt.Errorf("Invalid 'from' date")
	}

	if task.To == nil {
		return fmt.Errorf("Invalid 'to' date")
	}

	t.From = *task.From
	t.To = *task.To
	t.Weekdays = task.Weekdays
	t.Start = task.Start
	t.Door = task.Door
	t.Task = task.Task
	t.Cards = task.Cards

	return nil
}
