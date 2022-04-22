//go:build tests

package main

import (
	"C"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func getTimeProfile(uu uhppote.IUHPPOTE, deviceID uint32, profileID uint8) (*types.TimeProfile, error) {
	if deviceID != 405419896 {
		return nil, fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if profileID != 49 {
		return nil, fmt.Errorf("Incorrect profile ID (%v)", profileID)
	}

	from := types.ToDate(2022, time.February, 1)
	to := types.ToDate(2022, time.June, 30)

	profile := types.TimeProfile{
		ID:              49,
		LinkedProfileID: 71,
		From:            &from,
		To:              &to,

		Weekdays: map[time.Weekday]bool{
			time.Monday:    true,
			time.Tuesday:   false,
			time.Wednesday: true,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  false,
			time.Sunday:    true,
		},

		Segments: map[uint8]types.Segment{
			1: types.Segment{
				Start: types.NewHHmm(8, 30),
				End:   types.NewHHmm(11, 30),
			},
			2: types.Segment{
				Start: types.HHmm{},
				End:   types.HHmm{},
			},
			3: types.Segment{
				Start: types.HHmm{},
				End:   types.NewHHmm(18, 0),
			},
		},
	}

	return &profile, nil
}

func setTimeProfile(uu uhppote.IUHPPOTE, deviceID uint32, profile types.TimeProfile) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if profile.ID != 49 {
		return fmt.Errorf("Incorrect profile ID (%v)", profile.ID)
	}

	if profile.LinkedProfileID != 71 {
		return fmt.Errorf("Incorrect linked profile ID (%v)", profile.LinkedProfileID)
	}

	if from := fmt.Sprintf("%v", profile.From); from != "2022-02-01" {
		return fmt.Errorf("Incorrect 'from' date (%v)", from)
	}

	if to := fmt.Sprintf("%v", profile.To); to != "2022-06-30" {
		return fmt.Errorf("Incorrect 'to' date (%v)", to)
	}

	if !profile.Weekdays[time.Monday] {
		return fmt.Errorf("Incorrect Monday value (%v)", profile.Weekdays[time.Monday])
	}

	if profile.Weekdays[time.Tuesday] {
		return fmt.Errorf("Incorrect Tuesday value (%v)", profile.Weekdays[time.Tuesday])
	}

	if !profile.Weekdays[time.Wednesday] {
		return fmt.Errorf("Incorrect Wednesday value (%v)", profile.Weekdays[time.Wednesday])
	}

	if !profile.Weekdays[time.Thursday] {
		return fmt.Errorf("Incorrect Thursday value (%v)", profile.Weekdays[time.Thursday])
	}

	if profile.Weekdays[time.Friday] {
		return fmt.Errorf("Incorrect Friday value (%v)", profile.Weekdays[time.Friday])
	}

	if profile.Weekdays[time.Saturday] {
		return fmt.Errorf("Incorrect Saturday value (%v)", profile.Weekdays[time.Saturday])
	}

	if !profile.Weekdays[time.Sunday] {
		return fmt.Errorf("Incorrect Sunday value (%v)", profile.Weekdays[time.Sunday])
	}

	if start := fmt.Sprintf("%v", profile.Segments[1].Start); start != "08:30" {
		return fmt.Errorf("Incorrect segment 1 start (%v)", start)
	}

	if end := fmt.Sprintf("%v", profile.Segments[1].End); end != "11:30" {
		return fmt.Errorf("Incorrect segment 1 end (%v)", end)
	}

	if start := fmt.Sprintf("%v", profile.Segments[2].Start); start != "00:00" {
		return fmt.Errorf("Incorrect segment 2 start (%v)", start)
	}

	if end := fmt.Sprintf("%v", profile.Segments[2].End); end != "00:00" {
		return fmt.Errorf("Incorrect segment 2 end (%v)", end)
	}

	if start := fmt.Sprintf("%v", profile.Segments[3].Start); start != "00:00" {
		return fmt.Errorf("Incorrect segment 3 start (%v)", start)
	}

	if end := fmt.Sprintf("%v", profile.Segments[3].End); end != "18:00" {
		return fmt.Errorf("Incorrect segment 3 end (%v)", end)
	}

	return nil
}
