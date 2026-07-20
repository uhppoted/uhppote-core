package messages

import (
	"codeberg.org/uhppoted/uhppoted-core/types"
)

type SetAntiPassbackRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x84"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	AntiPassback uint8              `uhppote:"offset:8"`
}

type SetAntiPassbackResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x84"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Ok           bool               `uhppote:"offset:8"`
}
