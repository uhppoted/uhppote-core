package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type ActivateAccessKeypadsRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0xA4"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Reader1      bool               `uhppote:"offset:8"`
	Reader2      bool               `uhppote:"offset:9"`
	Reader3      bool               `uhppote:"offset:10"`
	Reader4      bool               `uhppote:"offset:11"`
}

type ActivateAccessKeypadsResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0xA4"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
