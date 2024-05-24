package uhppote

import (
	"fmt"
	"net/netip"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetListener(serialNumber uint32) (netip.AddrPort, error) {
	if serialNumber == 0 {
		return netip.AddrPort{}, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.GetListenerRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	if reply, err := sendto[messages.GetListenerResponse](u, serialNumber, request); err != nil {
		return netip.AddrPort{}, err
	} else {
		addr := reply.Address.To4()
		slice := [4]byte{addr[0], addr[1], addr[2], addr[3]}

		return netip.AddrPortFrom(netip.AddrFrom4(slice), reply.Port), nil
	}
}
