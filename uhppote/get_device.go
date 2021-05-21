package uhppote

import (
	"net"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) GetDevices() ([]types.Device, error) {
	request := messages.GetDeviceRequest{}

	replies, err := u.Broadcast(request, messages.GetDeviceResponse{})
	if err != nil {
		return nil, err
	}

	port := 60000
	if addr := u.BroadcastAddr(); addr != nil {
		port = addr.Port
	}

	devices := []types.Device{}
	for _, v := range replies {
		reply := v.(messages.GetDeviceResponse)

		name := ""
		if device, ok := u.devices[uint32(reply.SerialNumber)]; ok {
			name = device.Name
		}

		devices = append(devices, types.Device{
			Name:         name,
			SerialNumber: reply.SerialNumber,
			IpAddress:    reply.IpAddress,
			SubnetMask:   reply.SubnetMask,
			Gateway:      reply.Gateway,
			MacAddress:   reply.MacAddress,
			Version:      reply.Version,
			Date:         reply.Date,
			Address: net.UDPAddr{
				IP:   reply.IpAddress,
				Port: port,
				Zone: "",
			},
			TimeZone: time.Local,
		})
	}

	return devices, nil
}

func (u *uhppote) GetDevice(serialNumber uint32) (*types.Device, error) {
	request := messages.GetDeviceRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	replies, err := u.BroadcastTo(serialNumber, request, messages.GetDeviceResponse{})
	if err != nil {
		return nil, err
	}

	port := 60000
	if addr := u.BroadcastAddr(); addr != nil {
		port = addr.Port
	}

	if device, ok := u.DeviceList()[serialNumber]; ok {
		if device.Address != nil {
			port = device.Address.Port
		}
	}

	for _, v := range replies {
		reply := v.(messages.GetDeviceResponse)
		if uint32(reply.SerialNumber) == serialNumber {
			name := ""
			if device, ok := u.devices[serialNumber]; ok {
				name = device.Name
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
				Address: net.UDPAddr{
					IP:   reply.IpAddress,
					Port: port,
					Zone: "",
				},
				TimeZone: time.Local,
			}, nil
		}
	}

	return nil, nil
}
