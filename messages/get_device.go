package messages

import (
	"github.com/uhppoted/uhppote-core/types"
	"net"
)

type GetDeviceRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x94"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
}

type GetDeviceResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x94"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	IpAddress    net.IP             `uhppote:"offset:8"`
	SubnetMask   net.IP             `uhppote:"offset:12"`
	Gateway      net.IP             `uhppote:"offset:16"`
	MacAddress   types.MacAddress   `uhppote:"offset:20"`
	Version      types.Version      `uhppote:"offset:26"`
	Date         types.Date         `uhppote:"offset:28"`
}
