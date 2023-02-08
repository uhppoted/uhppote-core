package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type GetCardByIndexRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x5c"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Index        uint32             `uhppote:"offset:8"`
}

type GetCardByIDRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x5a"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	CardNumber   uint32             `uhppote:"offset:8"`
}

type GetCardByIndexResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x5c"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	CardNumber   uint32             `uhppote:"offset:8"`
	From         *types.Date        `uhppote:"offset:12"`
	To           *types.Date        `uhppote:"offset:16"`
	Door1        uint8              `uhppote:"offset:20"`
	Door2        uint8              `uhppote:"offset:21"`
	Door3        uint8              `uhppote:"offset:22"`
	Door4        uint8              `uhppote:"offset:23"`
	PIN          types.PIN          `uhppote:"offset:24"`
}

type GetCardByIDResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x5a"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	CardNumber   uint32             `uhppote:"offset:8"`
	From         *types.Date        `uhppote:"offset:12"`
	To           *types.Date        `uhppote:"offset:16"`
	Door1        uint8              `uhppote:"offset:20"`
	Door2        uint8              `uhppote:"offset:21"`
	Door3        uint8              `uhppote:"offset:22"`
	Door4        uint8              `uhppote:"offset:23"`
	PIN          types.PIN          `uhppote:"offset:24"`
}
