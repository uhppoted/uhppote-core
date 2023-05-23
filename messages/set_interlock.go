package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type SetInterlockRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0xa2"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Interlock    uint8              `uhppote:"offset:8"`
}

type SetInterlockResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0xa2"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
