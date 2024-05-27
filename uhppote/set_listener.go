package uhppote

import (
	"fmt"
	"net/netip"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) SetListener(controller uint32, address netip.AddrPort) (bool, error) {
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
	}

	if reply, err := sendto[messages.SetListenerResponse](u, controller, request); err != nil {
		return false, err
	} else if uint32(reply.SerialNumber) != controller {
		return false, ErrIncorrectController
	} else {
		return reply.Succeeded, nil
	}
}
