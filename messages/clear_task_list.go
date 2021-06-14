package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type ClearTaskListRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0xa6"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	MagicWord    uint32             `uhppote:"offset:8"`
}

type ClearTaskListResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0xa6"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
