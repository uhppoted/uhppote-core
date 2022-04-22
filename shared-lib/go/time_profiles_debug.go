//go:build debug

package main

import (
	"C"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func getTimeProfile(uu uhppote.IUHPPOTE, deviceID uint32, profileID uint8) (*types.TimeProfile, error) {
	if DEBUG {
		fmt.Printf(">>> get-time-profile\n")
		fmt.Printf("    ID:      %v\n", deviceID)
		fmt.Printf("    profile: %v\n", profileID)
		fmt.Println()
	}

	from := types.ToDate(2022, time.February, 1)
	to := types.ToDate(2022, time.June, 30)

	profile := types.TimeProfile{
		ID:              29,
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
	if DEBUG {
		fmt.Printf(">>> set-time-profile\n")
		fmt.Printf("    ID:                   %v\n", deviceID)
		fmt.Printf("    profile ID:           %v\n", profile.ID)
		fmt.Printf("    enabled    from:      %v\n", profile.From)
		fmt.Printf("               to:        %v\n", profile.To)
		fmt.Printf("    enabled on Monday:    %v\n", profile.Weekdays[time.Monday])
		fmt.Printf("               Tuesday:   %v\n", profile.Weekdays[time.Tuesday])
		fmt.Printf("               Wednesday: %v\n", profile.Weekdays[time.Wednesday])
		fmt.Printf("               Thursday:  %v\n", profile.Weekdays[time.Thursday])
		fmt.Printf("               Friday:    %v\n", profile.Weekdays[time.Friday])
		fmt.Printf("               Saturday:  %v\n", profile.Weekdays[time.Saturday])
		fmt.Printf("               Sunday:    %v\n", profile.Weekdays[time.Sunday])
		fmt.Printf("    segment 1:            %v-%v\n", profile.Segments[1].Start, profile.Segments[1].End)
		fmt.Printf("    segment 2:            %v-%v\n", profile.Segments[2].Start, profile.Segments[2].End)
		fmt.Printf("    segment 3:            %v-%v\n", profile.Segments[3].Start, profile.Segments[3].End)
		fmt.Println()
	}

	return nil
}
