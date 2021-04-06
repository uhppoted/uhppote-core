package types

import (
	"fmt"
	"net"
	"time"
)

type Device struct {
	SerialNumber SerialNumber
	IpAddress    net.IP
	SubnetMask   net.IP
	Gateway      net.IP
	MacAddress   MacAddress
	Version      Version
	Date         Date
	Address      net.UDPAddr
	TimeZone     *time.Location
}

func (device *Device) String() string {
	return fmt.Sprintf("%s %-15v %-15v %-15v %-17v %v %s",
		device.SerialNumber,
		device.IpAddress,
		device.SubnetMask,
		device.Gateway,
		device.MacAddress,
		device.Version,
		device.Date)
}
