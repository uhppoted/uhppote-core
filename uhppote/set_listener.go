package uhppote

import (
	"fmt"
	"net/netip"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

// Sets the controller event listener address:port and auto-send interval.
//
// The address must be either a valid IPv4 address and the port may not be 0 or
// 0.0.0.0:0.
//
// The interval is the interval in seconds at which the controller will repeatedly
// send the most recent event (in addition to events sent as as they occur). A zero interval
// (the default) will only send events on occurrence.
//
// Returns true if the controller event listener address was set, error if something
// the address:port was invalid or the controller did not respohd.
func (u *uhppote) SetListener(controller uint32, address netip.AddrPort, interval uint8) (bool, error) {
	if controller == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", controller)
	}

	if !address.IsValid() {
		return false, ErrInvalidListenerAddress
	}

	if (address != netip.MustParseAddrPort("0.0.0.0:0")) && (!address.Addr().Is4() || (address.Port() == 0)) {
		return false, fmt.Errorf("invalid listener address: %v", address)
	}

	request := messages.SetListenerRequest{
		SerialNumber: types.SerialNumber(controller),
		AddrPort:     address,
		Interval:     interval,
	}

	if reply, err := sendto[messages.SetListenerResponse](u, controller, request); err != nil {
		return false, err
	} else if uint32(reply.SerialNumber) != controller {
		return false, ErrIncorrectController
	} else {
		return reply.Succeeded, nil
	}
}
