package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type RefreshTaskListRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0xac"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	MagicWord    uint32             `uhppote:"offset:8"`
}

type RefreshTaskListResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0xac"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Refreshed    bool               `uhppote:"offset:8"`
}
