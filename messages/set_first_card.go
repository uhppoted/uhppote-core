package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type SetFirstCardRequest struct {
	MsgType          types.MsgType      `uhppote:"value:0xaa"`
	SerialNumber     types.SerialNumber `uhppote:"offset:4"`
	Door             uint8              `uhppote:"offset:8"`
	Start            types.HHmm         `uhppote:"offset:9"`
	StartDoorControl uint8              `uhppote:"offset:11"`
	End              types.HHmm         `uhppote:"offset:12"`
	EndDoorControl   uint8              `uhppote:"offset:14"`
	Monday           bool               `uhppote:"offset:15"`
	Tuesday          bool               `uhppote:"offset:16"`
	Wednesday        bool               `uhppote:"offset:17"`
	Thursday         bool               `uhppote:"offset:18"`
	Friday           bool               `uhppote:"offset:19"`
	Saturday         bool               `uhppote:"offset:20"`
	Sunday           bool               `uhppote:"offset:21"`
}

type SetFirstCardResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0xaa"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
