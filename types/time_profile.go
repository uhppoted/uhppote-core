package types

import (
	"encoding/json"
	"fmt"
	"strings"
)

type TimeProfile struct {
	ID              uint8    `json:"id"`
	LinkedProfileID uint8    `json:"linked-profile,omitempty"`
	From            Date     `json:"start-date"`
	To              Date     `json:"end-date"`
	Weekdays        Weekdays `json:"weekdays,omitempty"`
	Segments        Segments `json:"segments,omitempty"`
}

func (t TimeProfile) String() string {
	profile := fmt.Sprintf("%v", t.ID)
	linked := fmt.Sprintf("%v", t.LinkedProfileID)

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
	segments := fmt.Sprintf("%v", t.Segments)

	list := []string{profile}
	for _, s := range []string{dates, weekdays, segments, linked} {
		if s != "" {
			list = append(list, s)
		}
	}

	return strings.Join(list, " ")
}

func (t *TimeProfile) UnmarshalJSON(bytes []byte) error {
	profile := struct {
		ID              uint8    `json:"id"`
		LinkedProfileID uint8    `json:"linked-profile"`
		From            Date     `json:"start-date"`
		To              Date     `json:"end-date"`
		Weekdays        Weekdays `json:"weekdays"`
		Segments        Segments `json:"segments"`
	}{
		Weekdays: Weekdays{},
		Segments: Segments{},
	}

	profile.Segments[1] = Segment{Start: HHmm{}, End: HHmm{}}
	profile.Segments[2] = Segment{Start: HHmm{}, End: HHmm{}}
	profile.Segments[3] = Segment{Start: HHmm{}, End: HHmm{}}

	if err := json.Unmarshal(bytes, &profile); err != nil {
		return err
	}

	t.ID = profile.ID
	t.LinkedProfileID = profile.LinkedProfileID
	t.From = profile.From
	t.To = profile.To
	t.Weekdays = profile.Weekdays
	t.Segments = profile.Segments

	return nil
}
