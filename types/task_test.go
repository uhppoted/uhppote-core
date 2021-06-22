package types

import (
	"encoding/json"
	"fmt"
	"reflect"
	"testing"
	"time"
)

func TestTaskToString(t *testing.T) {
	expected := "ENABLE TIME PROFILE 3 2021-04-01:2021-12-29 Mon,Tue,Thurs,Sat,Sun 08:30"

	task := Task{
		Task: EnableTimeProfile,
		Door: 3,
		From: date("2021-04-01"),
		To:   date("2021-12-29"),
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Start: hhmm("08:30"),
		Cards: 13,
	}

	s := fmt.Sprintf("%v", task)

	if s != expected {
		t.Errorf("Task incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestTaskToStringWithEnableMoreCards(t *testing.T) {
	expected := "ENABLE MORE CARDS 3 2021-04-01:2021-12-29 Mon,Tue,Thurs,Sat,Sun 08:30 13"

	task := Task{
		Task: EnableMoreCards,
		Door: 3,
		From: date("2021-04-01"),
		To:   date("2021-12-29"),
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Start: hhmm("08:30"),
		Cards: 13,
	}

	s := fmt.Sprintf("%v", task)

	if s != expected {
		t.Errorf("Task incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestTaskJSONMarshal(t *testing.T) {
	expected := `{
  "task": "ENABLE TIME PROFILE",
  "door": 3,
  "start-date": "2021-04-01",
  "end-date": "2021-12-29",
  "weekdays": "Monday,Tuesday,Thursday,Saturday,Sunday",
  "start": "08:30",
  "cards": 13
}`

	task := Task{
		Task: EnableTimeProfile,
		Door: 3,
		From: date("2021-04-01"),
		To:   date("2021-12-29"),
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Start: hhmm("08:30"),
		Cards: 13,
	}

	bytes, err := json.MarshalIndent(task, "", "  ")
	if err != nil {
		t.Fatalf("Error marshalling Task (%v)", err)
	}

	if string(bytes) != expected {
		t.Errorf("Task incorrectly marshalled\n   expected:%v\n   got:     %v", expected, string(bytes))
	}
}

func TestTaskJSONUnmarshal(t *testing.T) {
	expected := Task{
		From: date("2021-04-01"),
		To:   date("2021-12-29"),
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Start: hhmm("08:30"),
		Door:  3,
		Task:  EnableTimeProfile,
		Cards: 23,
	}

	bytes := []byte(`{
  "start-date": "2021-04-01",
  "end-date": "2021-12-29",
  "weekdays": "Monday,Tuesday,Thursday,Saturday,Sunday",
  "start": "08:30",
  "door": 3,
  "task": 5,
  "cards": 23
}`)

	var profile Task

	if err := json.Unmarshal(bytes, &profile); err != nil {
		t.Fatalf("Error unmarshalling Task (%v)", err)
	}

	if !reflect.DeepEqual(profile, expected) {
		t.Errorf("Task incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, profile)
	}
}

func TestTaskTypeJSONUnmarshalFromNumber(t *testing.T) {
	expected := TriggerOnce
	bytes := []byte(`11`)

	var task TaskType

	if err := json.Unmarshal(bytes, &task); err != nil {
		t.Fatalf("Error unmarshalling TaskType - %v", err)
	}

	if !reflect.DeepEqual(task, expected) {
		t.Errorf("TaskType incorrectly unmarshalled - expected:%v, got:%v", expected, task)
	}
}

func TestTaskTypeJSONUnmarshalFromString(t *testing.T) {
	expected := TriggerOnce
	bytes := []byte(`"trigger once"`)

	var task TaskType

	if err := json.Unmarshal(bytes, &task); err != nil {
		t.Fatalf("Error unmarshalling TaskType - %v", err)
	}

	if !reflect.DeepEqual(task, expected) {
		t.Errorf("TaskType incorrectly unmarshalled - expected:%v, got:%v", expected, task)
	}
}
