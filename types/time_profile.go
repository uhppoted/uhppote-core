package types

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

type TimeProfile struct {
	ID              uint8    `json:"id"`
	LinkedProfileID uint8    `json:"linked-profile,omitempty"`
	From            *Date    `json:"start-date,omitempty"`
	To              *Date    `json:"end-date,omitempty"`
	Weekdays        Weekdays `json:"weekdays,omitempty"`
	Segments        Segments `json:"segments,omitempty"`
}

type Weekdays map[time.Weekday]bool

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
		From            *Date    `json:"start-date"`
		To              *Date    `json:"end-date"`
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

func (w Weekdays) String() string {
	days := []string{}
	if w != nil {
		for _, d := range []time.Weekday{time.Monday, time.Tuesday, time.Wednesday, time.Thursday, time.Friday, time.Saturday, time.Sunday} {
			if w[d] {
				days = append(days, abbreviation(d))
			}
		}
	}

	return strings.Join(days, ",")
}

func (w Weekdays) MarshalJSON() ([]byte, error) {
	s := []string{}

	for _, d := range [...]time.Weekday{time.Monday, time.Tuesday, time.Wednesday, time.Thursday, time.Friday, time.Saturday, time.Sunday} {
		if w[d] {
			s = append(s, fmt.Sprintf("%v", d))
		}
	}

	return json.Marshal(strings.Join(s, ","))
}

func (w *Weekdays) UnmarshalJSON(bytes []byte) error {
	var s string

	err := json.Unmarshal(bytes, &s)
	if err != nil {
		return err
	}

	(*w)[time.Monday] = false
	(*w)[time.Tuesday] = false
	(*w)[time.Wednesday] = false
	(*w)[time.Thursday] = false
	(*w)[time.Friday] = false
	(*w)[time.Saturday] = false
	(*w)[time.Sunday] = false

	tokens := strings.Split(s, ",")
	for _, t := range tokens {
		switch strings.ToLower(t) {
		case "monday":
			(*w)[time.Monday] = true
		case "tuesday":
			(*w)[time.Tuesday] = true
		case "wednesday":
			(*w)[time.Wednesday] = true
		case "thursday":
			(*w)[time.Thursday] = true
		case "friday":
			(*w)[time.Friday] = true
		case "saturday":
			(*w)[time.Saturday] = true
		case "sunday":
			(*w)[time.Sunday] = true
		}
	}

	return nil
}

func abbreviation(d time.Weekday) string {
	return [...]string{"Sun", "Mon", "Tue", "Wed", "Thurs", "Fri", "Sat"}[d]
}
