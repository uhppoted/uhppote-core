package uhppote

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) SetTimeProfile(deviceID uint32, profile types.TimeProfile) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	if profile.From == nil {
		return false, fmt.Errorf("time profile requires a valid 'from' date")
	}

	if profile.To == nil {
		return false, fmt.Errorf("time profile requires a valid 'to' date")
	}

	for _, k := range []uint8{1, 2, 3} {
		if segment, ok := profile.Segments[k]; !ok {
			return false, fmt.Errorf("time profile is missing segment %v", k)
		} else if segment.End.Before(segment.Start) {
			return false, fmt.Errorf("time profile segment %v end is before start (%v)", k, segment)
		}
	}

	request := messages.SetTimeProfileRequest{
		SerialNumber:    types.SerialNumber(deviceID),
		ProfileID:       profile.ID,
		From:            *profile.From,
		To:              *profile.To,
		Monday:          profile.Weekdays[time.Monday],
		Tuesday:         profile.Weekdays[time.Tuesday],
		Wednesday:       profile.Weekdays[time.Wednesday],
		Thursday:        profile.Weekdays[time.Thursday],
		Friday:          profile.Weekdays[time.Friday],
		Saturday:        profile.Weekdays[time.Saturday],
		Sunday:          profile.Weekdays[time.Sunday],
		Segment1Start:   profile.Segments[1].Start,
		Segment1End:     profile.Segments[1].End,
		Segment2Start:   profile.Segments[2].Start,
		Segment2End:     profile.Segments[2].End,
		Segment3Start:   profile.Segments[3].Start,
		Segment3End:     profile.Segments[3].End,
		LinkedProfileID: profile.LinkedProfileID,
	}

	if reply, err := sendto[messages.SetTimeProfileResponse](u, deviceID, request); err != nil {
		return false, err
	} else {
		return reply.Succeeded, nil
	}
}
