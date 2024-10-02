package messages

import (
	"github.com/uhppoted/uhppote-core/types"
	"net/netip"
)

type GetListenerRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x92"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
}

type GetListenerResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x92"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	AddrPort     netip.AddrPort     `uhppote:"offset:8"`
	Interval     uint8              `uhppote:"offset:14"`
}
