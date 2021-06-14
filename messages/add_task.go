package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type AddTaskRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0xa8"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	From         types.Date         `uhppote:"offset:8"`
	To           types.Date         `uhppote:"offset:12"`
	Monday       bool               `uhppote:"offset:16"`
	Tuesday      bool               `uhppote:"offset:17"`
	Wednesday    bool               `uhppote:"offset:18"`
	Thursday     bool               `uhppote:"offset:19"`
	Friday       bool               `uhppote:"offset:20"`
	Saturday     bool               `uhppote:"offset:21"`
	Sunday       bool               `uhppote:"offset:22"`
	Start        types.HHmm         `uhppote:"offset:23"`
	Door         uint8              `uhppote:"offset:25"`
	Task         uint8              `uhppote:"offset:26"`
	MoreCards    uint8              `uhppote:"offset:27"`
}

type AddTaskResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0xa8"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
