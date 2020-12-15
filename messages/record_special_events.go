package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type RecordSpecialEventsRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x8e"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Enable       bool               `uhppote:"offset:8"`
}

type RecordSpecialEventsResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x8e"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
