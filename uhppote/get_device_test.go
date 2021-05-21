package uhppote

import (
	"net"
	"reflect"
	"testing"
	"time"

	"github.com/uhppoted/uhppote-core/types"
)

func TestGetDevices(t *testing.T) {
	replies := [][]byte{
		{
			0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x08, 0x92, 0x20, 0x18, 0x08, 0x16,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		},
	}

	u := uhppote{
		driver: &stub{
			broadcast: func(request []byte, addr *net.UDPAddr) ([][]byte, error) {
				return replies, nil
			},
		},
	}

	MAC, _ := net.ParseMAC("00:66:19:39:55:2d")
	date, _ := time.ParseInLocation("20060102", "20180816", time.Local)
	expected := types.Device{
		SerialNumber: 423187757,
		IpAddress:    net.IPv4(192, 168, 0, 0),
		SubnetMask:   net.IPv4(255, 255, 255, 0),
		Gateway:      net.IPv4(0, 0, 0, 0),
		MacAddress:   types.MacAddress(MAC),
		Version:      0x0892,
		Date:         types.Date(date),
		Address: net.UDPAddr{
			IP:   net.IPv4(192, 168, 0, 0),
			Port: 60000,
			Zone: "",
		},
		TimeZone: time.Local,
	}

	response, err := u.GetDevices()
	if err != nil {
		t.Fatalf("Unexpected error returned from GetDevices (%v)", err)
	}

	if response == nil {
		t.Fatalf("Expected response from GetDevices, got:%v", response)
	}

	if len(response) != 1 {
		t.Fatalf("Expected one response from GetDevices, got:%v", len(response))
	}

	if response[0].SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' from valid message: %v\n", response[0].SerialNumber)
	}

	if !reflect.DeepEqual(response[0].IpAddress, expected.IpAddress) {
		t.Errorf("Incorrect 'IP address' from valid message: %v\n", response[0].IpAddress)
	}

	if !reflect.DeepEqual(response[0].SubnetMask, expected.SubnetMask) {
		t.Errorf("Incorrect 'subnet mask' from valid message: %v\n", response[0].SubnetMask)
	}

	if !reflect.DeepEqual(response[0].Gateway, expected.Gateway) {
		t.Errorf("Incorrect 'gateway' from valid message: %v\n", response[0].Gateway)
	}

	if !reflect.DeepEqual(response[0].MacAddress, expected.MacAddress) {
		t.Errorf("Incorrect 'MAC address' - expected:'%v', got:'%v'\n", expected.MacAddress, response[0].MacAddress)
	}

	if response[0].Version != expected.Version {
		t.Errorf("Incorrect 'version' from valid message: %v\n", response[0].Version)
	}

	if response[0].Date != expected.Date {
		t.Errorf("Incorrect 'date' from valid message: %v\n", response[0].Date)
	}

	if !reflect.DeepEqual(response[0].Address, expected.Address) {
		t.Errorf("Incorrect 'address' - expected:'%v', got:'%v'\n", expected.Address, response[0].Address)
	}

	if response[0].TimeZone != expected.TimeZone {
		t.Errorf("Incorrect 'timezone' from valid message: %v\n", response[0].TimeZone)
	}

	if !reflect.DeepEqual(response[0], expected) {
		t.Errorf("Invalid response:\nexpected:%#v\ngot:     %#v", expected, response[0])
	}
}

func TestGetDevicesWithAltPort(t *testing.T) {
	replies := [][]byte{
		{
			0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x08, 0x92, 0x20, 0x18, 0x08, 0x16,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		},
	}

	u := uhppote{
		broadcastAddr: &net.UDPAddr{
			IP:   net.IPv4(192, 168, 1, 255),
			Port: 54321,
		},

		driver: &stub{
			broadcast: func(request []byte, addr *net.UDPAddr) ([][]byte, error) {
				return replies, nil
			},
		},
	}

	MAC, _ := net.ParseMAC("00:66:19:39:55:2d")
	date, _ := time.ParseInLocation("20060102", "20180816", time.Local)
	expected := types.Device{
		SerialNumber: 423187757,
		IpAddress:    net.IPv4(192, 168, 0, 0),
		SubnetMask:   net.IPv4(255, 255, 255, 0),
		Gateway:      net.IPv4(0, 0, 0, 0),
		MacAddress:   types.MacAddress(MAC),
		Version:      0x0892,
		Date:         types.Date(date),
		Address: net.UDPAddr{
			IP:   net.IPv4(192, 168, 0, 0),
			Port: 54321,
			Zone: "",
		},
		TimeZone: time.Local,
	}

	response, err := u.GetDevices()
	if err != nil {
		t.Fatalf("Unexpected error returned from GetDevices (%v)", err)
	}

	if response == nil {
		t.Fatalf("Expected response from GetDevices, got:%v", response)
	}

	if len(response) != 1 {
		t.Fatalf("Expected one response from GetDevices, got:%v", len(response))
	}

	if response[0].SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' from valid message: %v\n", response[0].SerialNumber)
	}

	if !reflect.DeepEqual(response[0].IpAddress, expected.IpAddress) {
		t.Errorf("Incorrect 'IP address' from valid message: %v\n", response[0].IpAddress)
	}

	if !reflect.DeepEqual(response[0].SubnetMask, expected.SubnetMask) {
		t.Errorf("Incorrect 'subnet mask' from valid message: %v\n", response[0].SubnetMask)
	}

	if !reflect.DeepEqual(response[0].Gateway, expected.Gateway) {
		t.Errorf("Incorrect 'gateway' from valid message: %v\n", response[0].Gateway)
	}

	if !reflect.DeepEqual(response[0].MacAddress, expected.MacAddress) {
		t.Errorf("Incorrect 'MAC address' - expected:'%v', got:'%v'\n", expected.MacAddress, response[0].MacAddress)
	}

	if response[0].Version != expected.Version {
		t.Errorf("Incorrect 'version' from valid message: %v\n", response[0].Version)
	}

	if response[0].Date != expected.Date {
		t.Errorf("Incorrect 'date' from valid message: %v\n", response[0].Date)
	}

	if !reflect.DeepEqual(response[0].Address, expected.Address) {
		t.Errorf("Incorrect 'address' - expected:'%v', got:'%v'\n", expected.Address, response[0].Address)
	}

	if response[0].TimeZone != expected.TimeZone {
		t.Errorf("Incorrect 'timezone' from valid message: %v\n", response[0].TimeZone)
	}

	if !reflect.DeepEqual(response[0], expected) {
		t.Errorf("Invalid response:\nexpected:%#v\ngot:     %#v", expected, response[0])
	}
}

func TestGetDevice(t *testing.T) {
	replies := [][]byte{
		{
			0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x08, 0x92, 0x20, 0x18, 0x08, 0x16,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		},
	}

	u := uhppote{
		driver: &stub{
			broadcast: func(request []byte, addr *net.UDPAddr) ([][]byte, error) {
				return replies, nil
			},
		},
	}

	MAC, _ := net.ParseMAC("00:66:19:39:55:2d")
	date, _ := time.ParseInLocation("20060102", "20180816", time.Local)
	expected := types.Device{
		SerialNumber: 423187757,
		IpAddress:    net.IPv4(192, 168, 0, 0),
		SubnetMask:   net.IPv4(255, 255, 255, 0),
		Gateway:      net.IPv4(0, 0, 0, 0),
		MacAddress:   types.MacAddress(MAC),
		Version:      0x0892,
		Date:         types.Date(date),
		Address: net.UDPAddr{
			IP:   net.IPv4(192, 168, 0, 0),
			Port: 60000,
			Zone: "",
		},
		TimeZone: time.Local,
	}

	response, err := u.GetDevice(423187757)
	if err != nil {
		t.Fatalf("Unexpected error returned from GetDevice (%v)", err)
	}

	if response == nil {
		t.Fatalf("Expected response from GetDevice, got:%v", response)
	}

	if response.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' from valid message: %v\n", response.SerialNumber)
	}

	if !reflect.DeepEqual(response.IpAddress, expected.IpAddress) {
		t.Errorf("Incorrect 'IP address' from valid message: %v\n", response.IpAddress)
	}

	if !reflect.DeepEqual(response.SubnetMask, expected.SubnetMask) {
		t.Errorf("Incorrect 'subnet mask' from valid message: %v\n", response.SubnetMask)
	}

	if !reflect.DeepEqual(response.Gateway, expected.Gateway) {
		t.Errorf("Incorrect 'gateway' from valid message: %v\n", response.Gateway)
	}

	if !reflect.DeepEqual(response.MacAddress, expected.MacAddress) {
		t.Errorf("Incorrect 'MAC address' - expected:'%v', got:'%v'\n", expected.MacAddress, response.MacAddress)
	}

	if response.Version != expected.Version {
		t.Errorf("Incorrect 'version' from valid message: %v\n", response.Version)
	}

	if response.Date != expected.Date {
		t.Errorf("Incorrect 'date' from valid message: %v\n", response.Date)
	}

	if !reflect.DeepEqual(response.Address, expected.Address) {
		t.Errorf("Incorrect 'address' - expected:'%v', got:'%v'\n", expected.Address, response.Address)
	}

	if response.TimeZone != expected.TimeZone {
		t.Errorf("Incorrect 'timezone' from valid message: %v\n", response.TimeZone)
	}

	if !reflect.DeepEqual(*response, expected) {
		t.Errorf("Invalid response:\nexpected:%#v\ngot:     %#v", expected, *response)
	}
}

func TestGetDeviceWithAlternatePort(t *testing.T) {
	replies := [][]byte{
		{
			0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x08, 0x92, 0x20, 0x18, 0x08, 0x16,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		},
	}

	addr := net.UDPAddr{
		IP:   net.IPv4(192, 168, 0, 0),
		Port: 54321,
		Zone: "",
	}

	device := Device{
		Address: &addr,
	}

	u := uhppote{
		devices: map[uint32]Device{
			423187757: device,
		},

		driver: &stub{
			broadcast: func(request []byte, addr *net.UDPAddr) ([][]byte, error) {
				return replies, nil
			},
		},
	}

	MAC, _ := net.ParseMAC("00:66:19:39:55:2d")
	date, _ := time.ParseInLocation("20060102", "20180816", time.Local)
	expected := types.Device{
		SerialNumber: 423187757,
		IpAddress:    net.IPv4(192, 168, 0, 0),
		SubnetMask:   net.IPv4(255, 255, 255, 0),
		Gateway:      net.IPv4(0, 0, 0, 0),
		MacAddress:   types.MacAddress(MAC),
		Version:      0x0892,
		Date:         types.Date(date),
		Address: net.UDPAddr{
			IP:   net.IPv4(192, 168, 0, 0),
			Port: 54321,
			Zone: "",
		},
		TimeZone: time.Local,
	}

	response, err := u.GetDevice(423187757)
	if err != nil {
		t.Fatalf("Unexpected error returned from GetDevice (%v)", err)
	}

	if response == nil {
		t.Fatalf("Expected response from GetDevice, got:%v", response)
	}

	if response.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' from valid message: %v\n", response.SerialNumber)
	}

	if !reflect.DeepEqual(response.IpAddress, expected.IpAddress) {
		t.Errorf("Incorrect 'IP address' from valid message: %v\n", response.IpAddress)
	}

	if !reflect.DeepEqual(response.SubnetMask, expected.SubnetMask) {
		t.Errorf("Incorrect 'subnet mask' from valid message: %v\n", response.SubnetMask)
	}

	if !reflect.DeepEqual(response.Gateway, expected.Gateway) {
		t.Errorf("Incorrect 'gateway' from valid message: %v\n", response.Gateway)
	}

	if !reflect.DeepEqual(response.MacAddress, expected.MacAddress) {
		t.Errorf("Incorrect 'MAC address' - expected:'%v', got:'%v'\n", expected.MacAddress, response.MacAddress)
	}

	if response.Version != expected.Version {
		t.Errorf("Incorrect 'version' from valid message: %v\n", response.Version)
	}

	if response.Date != expected.Date {
		t.Errorf("Incorrect 'date' from valid message: %v\n", response.Date)
	}

	if !reflect.DeepEqual(response.Address, expected.Address) {
		t.Errorf("Incorrect 'address' - expected:'%v', got:'%v'\n", expected.Address, response.Address)
	}

	if response.TimeZone != expected.TimeZone {
		t.Errorf("Incorrect 'timezone' from valid message: %v\n", response.TimeZone)
	}

	if !reflect.DeepEqual(*response, expected) {
		t.Errorf("Invalid response:\nexpected:%#v\ngot:     %#v", expected, *response)
	}
}
