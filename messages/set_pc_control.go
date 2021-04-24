package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type SetPCControlRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0xA0"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	MagicWord    uint32             `uhppote:"offset:8"`
	Enable       bool               `uhppote:"offset:12"`
}

type SetPCControlResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0xA0"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
