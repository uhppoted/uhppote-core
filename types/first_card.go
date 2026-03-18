package types

import (
	"encoding/json"
	"fmt"
	"strings"
)

type FirstCard struct {
	From     HHmm         `json:"start-time"`
	To       HHmm         `json:"end-time"`
	Active   ControlState `json:"active-state"`
	Inactive ControlState `json:"inactive-state"`
	Weekdays Weekdays     `json:"weekdays,omitempty"`
}

func (f FirstCard) String() string {
	from := fmt.Sprintf("%v", f.From)
	to := fmt.Sprintf("%v", f.To)
	times := ""

	if from != "" && to != "" {
		times = from + "-" + to
	} else if from != "" {
		times = from + "-"
	} else {
		times = "-" + to
	}

	active := fmt.Sprintf("%4v", f.Active)
	inactive := fmt.Sprintf("%4v", f.Inactive)
	weekdays := fmt.Sprintf("%v", f.Weekdays)

	list := []string{}
	for _, s := range []string{"active:" + times, "control:" + active + "/" + inactive, "weekdays:" + weekdays} {
		if s != "" {
			list = append(list, s)
		}
	}

	return strings.Join(list, "  ")
}

func (f *FirstCard) UnmarshalJSON(bytes []byte) error {
	firstcard := struct {
		From     HHmm         `json:"start-time,omitempty"`
		To       HHmm         `json:"end-time,omitempty"`
		Active   ControlState `json:"active-state"`
		Inactive ControlState `json:"inactive-state"`
		Weekdays Weekdays     `json:"weekdays,omitempty"`
	}{
		Weekdays: Weekdays{},
	}

	if err := json.Unmarshal(bytes, &firstcard); err != nil {
		return err
	}

	f.From = firstcard.From
	f.To = firstcard.To
	f.Active = firstcard.Active
	f.Inactive = firstcard.Inactive
	f.Weekdays = firstcard.Weekdays

	return nil
}
