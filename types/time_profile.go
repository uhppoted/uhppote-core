package types

import (
	"fmt"
	"strings"
	"time"
)

type TimeProfile struct {
	ProfileID       uint8    `json:"profile-id"`
	LinkedProfileID uint8    `json:"linked-profile-id"`
	From            *Date    `json:"start-date"`
	To              *Date    `json:"end-date"`
	Weekdays        Weekdays `json:"weekdays"`
	Segments        Segments `json:"segments"`
}

type Weekdays map[Weekday]bool

type Weekday int

type Segments map[uint8]Segment

type Segment struct {
	Start *HHmm `json:"start"`
	End   *HHmm `json:"end"`
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
	profile := fmt.Sprintf("%v", t.ProfileID)
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

func (w Weekdays) String() string {
	days := []string{}
	if w != nil {
		for _, d := range []Weekday{Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday} {
			if w[d] {
				days = append(days, d.String())
			}
		}
	}

	return strings.Join(days, ",")
}

func (d Weekday) String() string {
	return [...]string{"Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun"}[d]
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
