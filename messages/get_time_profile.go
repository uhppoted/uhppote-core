package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type GetTimeProfileRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x98"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	ProfileID    uint8              `uhppote:"offset:8"`
}

type GetTimeProfileResponse struct {
	MsgType         types.MsgType      `uhppote:"value:0x98"`
	SerialNumber    types.SerialNumber `uhppote:"offset:4"`
	ProfileID       uint8              `uhppote:"offset:8"`
	From            *types.Date        `uhppote:"offset:9"`
	To              *types.Date        `uhppote:"offset:13"`
	Monday          bool               `uhppote:"offset:17"`
	Tuesday         bool               `uhppote:"offset:18"`
	Wednesday       bool               `uhppote:"offset:19"`
	Thursday        bool               `uhppote:"offset:20"`
	Friday          bool               `uhppote:"offset:21"`
	Saturday        bool               `uhppote:"offset:22"`
	Sunday          bool               `uhppote:"offset:23"`
	Segment1Start   *types.HHmm        `uhppote:"offset:24"`
	Segment1End     *types.HHmm        `uhppote:"offset:26"`
	Segment2Start   *types.HHmm        `uhppote:"offset:28"`
	Segment2End     *types.HHmm        `uhppote:"offset:30"`
	Segment3Start   *types.HHmm        `uhppote:"offset:32"`
	Segment3End     *types.HHmm        `uhppote:"offset:34"`
	LinkedProfileID uint8              `uhppote:"offset:36"`
}
