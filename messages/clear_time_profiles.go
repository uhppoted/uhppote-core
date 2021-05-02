package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type ClearTimeProfilesRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x8a"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	MagicWord    uint32             `uhppote:"offset:8"`
}

type ClearTimeProfilesResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x8a"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
