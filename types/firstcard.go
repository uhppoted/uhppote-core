package types

import (
	"encoding/json"
	"fmt"
	"strings"
)

type FirstCard struct {
	StartTime HHmm         `json:"start-time"`
	EndTime   HHmm         `json:"end-time"`
	Active    ControlState `json:"active-state"`
	Inactive  ControlState `json:"inactive-state"`
	Weekdays  Weekdays     `json:"weekdays,omitempty"`
}

func (f FirstCard) String() string {
	start := fmt.Sprintf("%v", f.StartTime)
	end := fmt.Sprintf("%v", f.EndTime)
	times := ""

	if start != "" && end != "" {
		times = start + "-" + end
	} else if start != "" {
		times = start + "-"
	} else {
		times = "-" + end
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
		StartTime HHmm         `json:"start-time,omitempty"`
		EndTime   HHmm         `json:"end-time,omitempty"`
		Active    ControlState `json:"active-state"`
		Inactive  ControlState `json:"inactive-state"`
		Weekdays  Weekdays     `json:"weekdays,omitempty"`
	}{
		Weekdays: Weekdays{},
	}

	if err := json.Unmarshal(bytes, &firstcard); err != nil {
		return err
	}

	f.StartTime = firstcard.StartTime
	f.EndTime = firstcard.EndTime
	f.Active = firstcard.Active
	f.Inactive = firstcard.Inactive
	f.Weekdays = firstcard.Weekdays

	return nil
}
