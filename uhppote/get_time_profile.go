package uhppote

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetTimeProfile(deviceID uint32, profileID uint8) (*types.TimeProfile, error) {
	if deviceID == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.GetTimeProfileRequest{
		SerialNumber: types.SerialNumber(deviceID),
		ProfileID:    profileID,
	}

	if reply, err := sendto[messages.GetTimeProfileResponse](u, deviceID, request); err != nil {
		return nil, err
	} else {
		if reply.ProfileID != 0 && reply.ProfileID != profileID {
			return nil, fmt.Errorf("incorrect profile ID in response - expected '%v', received '%v'", profileID, reply.ProfileID)
		}

		// 0: not active
		if reply.ProfileID == 0 {
			return nil, nil
		}

		// NTS: nil pointer replaced with zero value and zero value is theoretically possible (albeit invalid)
		// if reply.From == nil {
		// 	return nil, fmt.Errorf("invalid 'from' date in response")
		// }

		// NTS: nil pointer replaced with zero value and zero value is theoretically possible (albeit invalid)
		// if reply.To == nil {
		// 	return nil, fmt.Errorf("invalid 'to' date in response")
		// }

		segments := []types.Segment{types.Segment{}, types.Segment{}, types.Segment{}}

		if reply.Segment1Start != nil {
			segments[0].Start = *reply.Segment1Start
		}

		if reply.Segment1End != nil {
			segments[0].End = *reply.Segment1End
		}

		if reply.Segment2Start != nil {
			segments[1].Start = *reply.Segment2Start
		}

		if reply.Segment2End != nil {
			segments[1].End = *reply.Segment2End
		}

		if reply.Segment3Start != nil {
			segments[2].Start = *reply.Segment3Start
		}

		if reply.Segment3End != nil {
			segments[2].End = *reply.Segment3End
		}

		profile := types.TimeProfile{
			ID:              reply.ProfileID,
			LinkedProfileID: reply.LinkedProfileID,
			From:            reply.From,
			To:              reply.To,

			Weekdays: types.Weekdays{
				time.Monday:    reply.Monday,
				time.Tuesday:   reply.Tuesday,
				time.Wednesday: reply.Wednesday,
				time.Thursday:  reply.Thursday,
				time.Friday:    reply.Friday,
				time.Saturday:  reply.Saturday,
				time.Sunday:    reply.Sunday,
			},

			Segments: types.Segments{
				1: segments[0],
				2: segments[1],
				3: segments[2],
			},
		}

		return &profile, nil
	}
}
