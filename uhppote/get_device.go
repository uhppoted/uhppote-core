package uhppote

import (
	"net"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) GetDevices() ([]types.Device, error) {
	driver := iuhppote(u)
	if u.driver != nil {
		driver = u.driver
	}

	request := messages.GetDeviceRequest{}

	replies, err := driver.Broadcast(request, messages.GetDeviceResponse{})
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
		devices = append(devices, types.Device{
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

func (u *UHPPOTE) GetDevice(serialNumber uint32) (*types.Device, error) {
	driver := iuhppote(u)
	if u.driver != nil {
		driver = u.driver
	}

	request := messages.GetDeviceRequest{
		SerialNumber: types.SerialNumber(serialNumber),
	}

	replies, err := driver.BroadcastTo(serialNumber, request, messages.GetDeviceResponse{})
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
			return &types.Device{
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
