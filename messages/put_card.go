package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type PutCardRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x50"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	CardNumber   uint32             `uhppote:"offset:8"`
	From         types.Date         `uhppote:"offset:12"`
	To           types.Date         `uhppote:"offset:16"`
	Door1        uint8              `uhppote:"offset:20"`
	Door2        uint8              `uhppote:"offset:21"`
	Door3        uint8              `uhppote:"offset:22"`
	Door4        uint8              `uhppote:"offset:23"`
}

type PutCardResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x50"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
