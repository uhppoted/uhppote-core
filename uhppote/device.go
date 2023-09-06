package uhppote

import (
	"net"
	"time"
)

// Configuration information for an access controller declared in the common
// uhppoted.conf configuration.
type Device struct {
	Name     string         // controller name
	DeviceID uint32         // controller serial number
	Address  *net.UDPAddr   // controller IPv4 address
	Doors    []string       // controller door names (required for ACL functions)
	TimeZone *time.Location // controller timezone (required when controller is located in a different time zone to the application)
}

// Convenience function to instantiate a configured controller from the information in a configuration file.
func NewDevice(name string, serialNumber uint32, address *net.UDPAddr, doors []string) *Device {
	return &Device{
		Name:     name,
		DeviceID: serialNumber,
		Address:  address,
		Doors:    doors,
		TimeZone: time.Local,
	}
}

// Returns a deep copy of the configured controller information.
func (d Device) Clone() Device {
	device := Device{
		Name:     d.Name,
		DeviceID: d.DeviceID,
		Address:  nil,
		Doors:    make([]string, len(d.Doors)),
		TimeZone: d.TimeZone,
	}

	if d.Address != nil {
		device.Address = &net.UDPAddr{
			IP:   d.Address.IP.To4(),
			Port: d.Address.Port,
			Zone: "",
		}
	}

	copy(device.Doors, d.Doors)

	return device
}

// Safe function to get a controller serial number - returns 0 if the
// device is nil.
func (d *Device) ID() uint32 {
	if d != nil {
		return d.DeviceID
	}

	return 0
}
