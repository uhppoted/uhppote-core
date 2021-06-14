package types

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

type Weekdays map[time.Weekday]bool

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
