package types

import (
	"fmt"
	"net"
	"regexp"
	"strings"
	"time"
)

type Device struct {
	Name         string
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
	format := "%[2]s %-15[3]v %-15[4]v %-15[5]v %-17[6]v %[7]v %[8]s"
	if device.Name != "" {
		format = "%[1]s  %[2]s %-15[3]v %-15[4]v %-15[5]v %-17[6]v %[7]v %[8]s"
	}

	return fmt.Sprintf(format,
		regexp.MustCompile(`\s+`).ReplaceAllString(strings.TrimSpace(device.Name), " "),
		device.SerialNumber,
		device.IpAddress,
		device.SubnetMask,
		device.Gateway,
		device.MacAddress,
		device.Version,
		device.Date)
}
