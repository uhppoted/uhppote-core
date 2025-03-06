package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type SetAntiPassbackRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x84"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Mode         uint8              `uhppote:"offset:8"`
}

type SetAntiPassbackResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x84"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Ok           bool               `uhppote:"offset:8"`
}
