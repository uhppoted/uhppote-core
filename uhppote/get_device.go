package uhppote

import (
	"fmt"
	"net/netip"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetDevices() ([]types.Device, error) {
	request := messages.GetDeviceRequest{}

	replies, err := u.broadcast(request, messages.GetDeviceResponse{})
	if err != nil {
		return nil, err
	}

	port := uint16(60000)
	if u.broadcastAddr.IsValid() {
		port = u.broadcastAddr.Port()
	}

	controllers := []types.Device{}
	for _, v := range replies {
		reply := v.(messages.GetDeviceResponse)

		name := ""
		if device, ok := u.devices[uint32(reply.SerialNumber)]; ok {
			name = device.Name
		}

		addr := netip.AddrPort{}
		if v, ok := netip.AddrFromSlice(reply.IpAddress.To4()); ok {
			addr = netip.AddrPortFrom(v, port)
		}

		controllers = append(controllers, types.Device{
			Name:         name,
			SerialNumber: reply.SerialNumber,
			IpAddress:    reply.IpAddress,
			SubnetMask:   reply.SubnetMask,
			Gateway:      reply.Gateway,
			MacAddress:   reply.MacAddress,
			Version:      reply.Version,
			Date:         reply.Date,
			Address:      addr,
			TimeZone:     time.Local,
		})
	}

	return controllers, nil
}

func (u *uhppote) GetDevice(serialNumber uint32) (*types.Device, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid device ID (%v)", serialNumber)
	}

	request := messages.GetDeviceRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	if reply, err := sendto[messages.GetDeviceResponse](u, serialNumber, request); err != nil {
		return nil, err
	} else {
		name := ""
		if device, ok := u.devices[serialNumber]; ok {
			name = device.Name
		}

		port := uint16(60000)
		if u.broadcastAddr.IsValid() {
			port = u.broadcastAddr.Port()
		}

		if device, ok := u.devices[serialNumber]; ok {
			if device.Address.IsValid() {
				port = device.Address.Port()
			}
		}

		addr := netip.AddrPort{}
		if v, ok := netip.AddrFromSlice(reply.IpAddress.To4()); ok {
			addr = netip.AddrPortFrom(v, port)
		}

		return &types.Device{
			Name:         name,
			SerialNumber: reply.SerialNumber,
			IpAddress:    reply.IpAddress,
			SubnetMask:   reply.SubnetMask,
			Gateway:      reply.Gateway,
			MacAddress:   reply.MacAddress,
			Version:      reply.Version,
			Date:         reply.Date,
			Address:      addr,
			TimeZone:     time.Local,
		}, nil
	}
}
