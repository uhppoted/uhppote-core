package uhppote

import (
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) SetTimeProfile(deviceID uint32, profile types.TimeProfile) (bool, error) {
	driver := iuhppote(u)
	if u.driver != nil {
		driver = u.driver
	}

	if profile.From == nil {
		return false, fmt.Errorf("Time profile requires a valid 'from' date")
	}

	if profile.To == nil {
		return false, fmt.Errorf("Time profile requires a valid 'to' date")
	}

	for _, k := range []uint8{1, 2, 3} {
		if segment, ok := profile.Segments[k]; !ok {
			return false, fmt.Errorf("Time profile is missing segment %v", k)
		} else if segment.Start == nil {
			return false, fmt.Errorf("Time profile requires a valid segment %v 'start' time", k)
		} else if segment.End == nil {
			return false, fmt.Errorf("Time profile requires a valid segment %v 'end' time", k)
		}
	}

	request := messages.SetTimeProfileRequest{
		SerialNumber:    types.SerialNumber(deviceID),
		ProfileID:       profile.ID,
		From:            *profile.From,
		To:              *profile.To,
		Monday:          profile.Weekdays[types.Monday],
		Tuesday:         profile.Weekdays[types.Tuesday],
		Wednesday:       profile.Weekdays[types.Wednesday],
		Thursday:        profile.Weekdays[types.Thursday],
		Friday:          profile.Weekdays[types.Friday],
		Saturday:        profile.Weekdays[types.Saturday],
		Sunday:          profile.Weekdays[types.Sunday],
		Segment1Start:   *profile.Segments[1].Start,
		Segment1End:     *profile.Segments[1].End,
		Segment2Start:   *profile.Segments[2].Start,
		Segment2End:     *profile.Segments[2].End,
		Segment3Start:   *profile.Segments[3].Start,
		Segment3End:     *profile.Segments[3].End,
		LinkedProfileID: profile.LinkedProfileID,
	}

	response := messages.SetTimeProfileResponse{}

	err := driver.Send(deviceID, request, &response)
	if err != nil {
		return false, err
	}

	if uint32(response.SerialNumber) != deviceID {
		return false, fmt.Errorf("Incorrect device ID in response - expected '%v', received '%v'", deviceID, response.SerialNumber)
	}

	return response.Succeeded, nil
}
