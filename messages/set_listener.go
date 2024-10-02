package messages

import (
	"github.com/uhppoted/uhppote-core/types"
	"net/netip"
)

type SetListenerRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x90"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	AddrPort     netip.AddrPort     `uhppote:"offset:8"`
	Interval     uint8              `uhppote:"offset:14"`
}

type SetListenerResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x90"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
