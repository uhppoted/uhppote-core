package uhppote

import (
	"net"
	"time"
)

type Device struct {
	Name     string
	DeviceID uint32
	Address  *net.UDPAddr
	Rollover uint32
	Doors    []string
	TimeZone *time.Location
}

func NewDevice(name string, serialNumber uint32, address *net.UDPAddr, rollover uint32, doors []string) *Device {
	return &Device{
		Name:     name,
		DeviceID: serialNumber,
		Address:  address,
		Rollover: rollover,
		Doors:    doors,
		TimeZone: time.Local,
	}
}

func (d *Device) ID() uint32 {
	if d != nil {
		return d.DeviceID
	}

	return 0
}

func (d *Device) RolloverAt() uint32 {
	if d != nil {
		return d.Rollover
	}

	return 100000
}
