package uhppote

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetTimeProfile(deviceID uint32, profileID uint8) (*types.TimeProfile, error) {
	request := messages.GetTimeProfileRequest{
		SerialNumber: types.SerialNumber(deviceID),
		ProfileID:    profileID,
	}

	response := messages.GetTimeProfileResponse{}

	err := u.impl.Send(deviceID, request, &response)
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
			1: types.Segment{
				Start: response.Segment1Start,
				End:   response.Segment1End,
			},
			2: types.Segment{
				Start: response.Segment2Start,
				End:   response.Segment2End,
			},
			3: types.Segment{
				Start: response.Segment3Start,
				End:   response.Segment3End,
			},
		},
	}

	return &profile, nil
}
