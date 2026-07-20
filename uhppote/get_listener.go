package uhppote

import (
	"fmt"
	"net/netip"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

// Retrieves the configured events listener address:port and auto-send interval from
// the controller.
//
// Returns an error if something the controller did not respohd.
func (u *uhppote) GetListener(serialNumber uint32) (netip.AddrPort, uint8, error) {
	if serialNumber == 0 {
		return netip.AddrPort{}, 0, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.GetListenerRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	if reply, err := sendto[messages.GetListenerResponse](u, serialNumber, request); err != nil {
		return netip.AddrPort{}, 0, err
	} else {
		return reply.AddrPort, reply.Interval, nil
	}
}
