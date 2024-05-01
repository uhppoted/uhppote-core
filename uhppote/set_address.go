package uhppote

import (
	"fmt"
	"net"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) SetAddress(serialNumber uint32, address, mask, gateway net.IP) (*types.Result, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	if address.To4() == nil {
		return nil, fmt.Errorf("invalid IP address: %v", address)
	}

	if mask.To4() == nil {
		return nil, fmt.Errorf("invalid subnet mask: %v", mask)
	}

	if gateway.To4() == nil {
		return nil, fmt.Errorf("invalid gateway address: %v", gateway)
	}

	request := messages.SetAddressRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		Address:      address,
		Mask:         mask,
		Gateway:      gateway,
		MagicWord:    0x55aaaa55,
	}

	// UT0311-L04 doesn't seem to send a response. The reported remote IP address doesn't change on subsequent commands
	// (both internally and onl Wireshark) but the UT0311-L04 only replies to ping's on the new IP address. Wireshark
	// reports a 'Gratuitous ARP request' which looks correct after a set-address. Might be something to do with the
	// TPLink or MacOS ARP implementation.
	if _, err := sendto[none](u, serialNumber, request); err != nil {
		return nil, err
	} else {
		return &types.Result{
			SerialNumber: types.SerialNumber(serialNumber),
			Succeeded:    true,
		}, nil
	}
}
