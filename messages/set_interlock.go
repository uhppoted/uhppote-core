package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type InterlockMode uint8

const (
	Disabled    InterlockMode = 0x00 // disabled
	Reader12_34 InterlockMode = 0x01 // reader (1,2) and (3,4)
	Reader13_24 InterlockMode = 0x02 // reader (1,3) and (2,4)
	Reader123   InterlockMode = 0x03 // reader (1,2,3)
	Reader1234  InterlockMode = 0x04 // reader (1,2,3,4)
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
