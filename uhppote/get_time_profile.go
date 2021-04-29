package uhppote

import (
	"fmt"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) GetTimeProfile(deviceID uint32, profileID uint8) (*types.TimeProfile, error) {
	request := messages.GetTimeProfileRequest{
		SerialNumber: types.SerialNumber(deviceID),
		ProfileID:    profileID,
	}

	response := messages.GetTimeProfileResponse{}
	driver := iuhppote(u)
	if u.driver != nil {
		driver = u.driver
	}

	err := driver.Send(deviceID, request, &response)
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

	return &types.TimeProfile{
		ProfileID:       response.ProfileID,
		LinkedProfileID: response.LinkedProfileID,
		From:            response.From,
		To:              response.To,

		Weekdays: struct {
			Monday    bool `json:"monday"`
			Tuesday   bool `json:"tuesday"`
			Wednesday bool `json:"wednesday"`
			Thursday  bool `json:"thursday"`
			Friday    bool `json:"friday"`
			Saturday  bool `json:"saturday"`
			Sunday    bool `json:"sunday"`
		}{
			Monday:    response.Monday,
			Tuesday:   response.Tuesday,
			Wednesday: response.Wednesday,
			Thursday:  response.Thursday,
			Friday:    response.Friday,
			Saturday:  response.Saturday,
			Sunday:    response.Sunday,
		},

		Segments: map[uint8]struct {
			Start *types.HHmm `json:"start"`
			End   *types.HHmm `json:"end"`
		}{
			1: struct {
				Start *types.HHmm `json:"start"`
				End   *types.HHmm `json:"end"`
			}{
				Start: response.Segment1Start,
				End:   response.Segment1End,
			},
			2: struct {
				Start *types.HHmm `json:"start"`
				End   *types.HHmm `json:"end"`
			}{
				Start: response.Segment2Start,
				End:   response.Segment2End,
			},
			3: struct {
				Start *types.HHmm `json:"start"`
				End   *types.HHmm `json:"end"`
			}{
				Start: response.Segment3Start,
				End:   response.Segment3End,
			},
		},
	}, nil
}
