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

type Weekdays map[Weekday]bool

type Weekday int

type Segments map[uint8]Segment

type Segment struct {
	Start *HHmm `json:"start,omitempty"`
	End   *HHmm `json:"end,omitempty"`
}

const (
	Monday Weekday = iota
	Tuesday
	Wednesday
	Thursday
	Friday
	Saturday
	Sunday
)

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
		for _, d := range []Weekday{Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday} {
			if w[d] {
				days = append(days, d.Abbreviation())
			}
		}
	}

	return strings.Join(days, ",")
}

func (w Weekdays) MarshalJSON() ([]byte, error) {
	s := []string{}

	for _, d := range [...]Weekday{Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday} {
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

	(*w)[Monday] = false
	(*w)[Tuesday] = false
	(*w)[Wednesday] = false
	(*w)[Thursday] = false
	(*w)[Friday] = false
	(*w)[Saturday] = false
	(*w)[Sunday] = false

	tokens := strings.Split(s, ",")
	for _, t := range tokens {
		switch strings.ToLower(t) {
		case "monday":
			(*w)[Monday] = true
		case "tuesday":
			(*w)[Tuesday] = true
		case "wednesday":
			(*w)[Wednesday] = true
		case "thursday":
			(*w)[Thursday] = true
		case "friday":
			(*w)[Friday] = true
		case "saturday":
			(*w)[Saturday] = true
		case "sunday":
			(*w)[Sunday] = true
		}
	}

	return nil
}

func (d Weekday) String() string {
	return [...]string{"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"}[d]
}

func (d Weekday) Abbreviation() string {
	return [...]string{"Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun"}[d]
}

func (d Weekday) MarshalJSON() ([]byte, error) {
	s := [...]string{"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"}[d]

	return json.Marshal(s)
}

func (d *Weekday) UnmarshalJSON(bytes []byte) error {
	var s string

	err := json.Unmarshal(bytes, &s)
	if err != nil {
		return err
	}

	switch strings.ToLower(s) {
	case "monday":
		*d = Monday
	case "tuesday":
		*d = Tuesday
	case "wednesday":
		*d = Wednesday
	case "thursday":
		*d = Thursday
	case "friday":
		*d = Friday
	case "saturday":
		*d = Saturday
	case "sunday":
		*d = Sunday
	default:
		return fmt.Errorf("Invalid weekday (%v)", string(bytes))
	}

	return nil
}

func (ss Segments) String() string {
	segments := []string{}
	for _, ix := range []uint8{1, 2, 3} {
		if s := fmt.Sprintf("%v", ss[ix]); s != "" {
			segments = append(segments, s)
		}
	}

	return strings.Join(segments, ",")
}

func (s Segment) String() string {
	start := time.Date(0, time.January, 0, 0, 0, 0, 0, time.Local)
	end := time.Date(0, time.January, 0, 0, 0, 0, 0, time.Local)

	if s.Start != nil {
		start = time.Time(*s.Start)
	}

	if s.End != nil {
		end = time.Time(*s.End)
	}

	if start.Hour() == 0 && start.Minute() == 0 && end.Hour() == 0 && end.Minute() == 0 {
		return ""
	}

	return fmt.Sprintf("%v-%v", s.Start, s.End)
}
