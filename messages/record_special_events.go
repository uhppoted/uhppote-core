package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

// Request message layout for 'record special events' (function code 0x8e).
// Enables or disables events for door open, door closed and door button
// pressed.
type RecordSpecialEventsRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x8e"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Enable       bool               `uhppote:"offset:8"`
}

// Response message layout for 'record special events' (function code 0x8e).
type RecordSpecialEventsResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x8e"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
