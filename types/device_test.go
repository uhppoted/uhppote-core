package types

import (
	"net"
	"testing"
)

func TestDeviceString(t *testing.T) {
	MAC, _ := net.ParseMAC("00:66:19:39:55:2d")
	released, _ := DateFromString("2020-12-05")

	device := Device{
		SerialNumber: 405419896,
		IpAddress:    net.IPv4(10, 0, 0, 100),
		SubnetMask:   net.IPv4(255, 255, 255, 0),
		Gateway:      net.IPv4(10, 0, 0, 1),
		MacAddress:   MacAddress(MAC),
		Version:      0x0892,
		Date:         *released,
	}

	expected := "405419896  10.0.0.100      255.255.255.0   10.0.0.1        00:66:19:39:55:2d 8.92 2020-12-05"
	s := device.String()

	if s != expected {
		t.Errorf("Invalid return from Device.String()\n   expected:%s\n   got:     %s", expected, s)
	}
}
