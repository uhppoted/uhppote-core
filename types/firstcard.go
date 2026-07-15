package types

import (
	"encoding/json"
	"fmt"
	"slices"
	"strings"
	"time"
)

type FirstCard struct {
	StartTime HHmm         `json:"start-time"`
	EndTime   HHmm         `json:"end-time"`
	Active    ControlState `json:"active-mode,omitempty"`
	Inactive  ControlState `json:"inactive-mode,omitempty"`
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

// Returns true if the both start and end times are 00:00 and none of the weekdays
// is enabled.
func (f FirstCard) IsZero() bool {
	all := []bool{
		!f.StartTime.Equals(HHmm{}),
		!f.EndTime.Equals(HHmm{}),
		f.Weekdays[time.Monday],
		f.Weekdays[time.Tuesday],
		f.Weekdays[time.Wednesday],
		f.Weekdays[time.Thursday],
		f.Weekdays[time.Friday],
		f.Weekdays[time.Saturday],
		f.Weekdays[time.Sunday],
	}

	return !slices.Contains(all, true)
}

func (f *FirstCard) UnmarshalJSON(bytes []byte) error {
	firstcard := struct {
		StartTime HHmm         `json:"start-time"`
		EndTime   HHmm         `json:"end-time"`
		Active    ControlState `json:"active-mode"`
		Inactive  ControlState `json:"inactive-mode"`
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
