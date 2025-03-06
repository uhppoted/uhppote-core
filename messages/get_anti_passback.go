package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type GetAntiPassbackRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x86"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
}

type GetAntiPassbackResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x86"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Mode         uint8              `uhppote:"offset:8"`
}
