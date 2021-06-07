package uhppote

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetTimeProfile(deviceID uint32, profileID uint8) (*types.TimeProfile, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("Invalid device ID (%v)", deviceID)
	}

	request := messages.GetTimeProfileRequest{
		SerialNumber: types.SerialNumber(deviceID),
		ProfileID:    profileID,
	}

	response := messages.GetTimeProfileResponse{}

	err := u.send(deviceID, request, &response)
	if err != nil {
		return nil, err
	}

	if uint32(response.SerialNumber) != deviceID {
		return nil, fmt.Errorf("Incorrect device ID in response - expected '%v', received '%v'", deviceID, response.SerialNumber)
	}

	if response.ProfileID != 0 && response.ProfileID != profileID {
		return nil, fmt.Errorf("Incorrect profile ID in response - expected '%v', received '%v'", profileID, response.ProfileID)
	}

	// 0: not active
	if response.ProfileID == 0 {
		return nil, nil
	}

	if response.From == nil {
		return nil, fmt.Errorf("Invalid 'from' date in response")
	}

	if response.To == nil {
		return nil, fmt.Errorf("Invalid 'to' date in response")
	}

	segments := []types.Segment{types.Segment{}, types.Segment{}, types.Segment{}}

	if response.Segment1Start != nil {
		segments[0].Start = *response.Segment1Start
	}

	if response.Segment1End != nil {
		segments[0].End = *response.Segment1End
	}

	if response.Segment2Start != nil {
		segments[1].Start = *response.Segment2Start
	}

	if response.Segment2End != nil {
		segments[1].End = *response.Segment2End
	}

	if response.Segment3Start != nil {
		segments[2].Start = *response.Segment3Start
	}

	if response.Segment3End != nil {
		segments[2].End = *response.Segment3End
	}

	profile := types.TimeProfile{
		ID:              response.ProfileID,
		LinkedProfileID: response.LinkedProfileID,
		From:            response.From,
		To:              response.To,

		Weekdays: types.Weekdays{
			time.Monday:    response.Monday,
			time.Tuesday:   response.Tuesday,
			time.Wednesday: response.Wednesday,
			time.Thursday:  response.Thursday,
			time.Friday:    response.Friday,
			time.Saturday:  response.Saturday,
			time.Sunday:    response.Sunday,
		},

		Segments: types.Segments{
			1: segments[0],
			2: segments[1],
			3: segments[2],
		},
	}

	return &profile, nil
}
