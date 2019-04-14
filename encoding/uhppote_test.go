package uhppote

import (
	"encoding/hex"
	"fmt"
	"net"
	"reflect"
	"regexp"
	"testing"
	"time"
	"uhppote/types"
)

func TestMarshal(t *testing.T) {
	expected := []byte{
		0x17, 0x58, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x01, 0x02, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x92, 0x20, 0x18, 0x08, 0x16,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	mac, _ := net.ParseMAC("00:66:19:39:55:2d")
	date, _ := time.ParseInLocation("20060102", "20180816", time.Local)

	request := struct {
		MsgType      byte               `uhppote:"offset:1"`
		Uint32       uint32             `uhppote:"offset:4"`
		Address      net.IP             `uhppote:"offset:8"`
		MacAddress   net.HardwareAddr   `uhppote:"offset:16"`
		SerialNumber types.SerialNumber `uhppote:"offset:22"`
		Version      types.Version      `uhppote:"offset:26"`
		Date         types.Date         `uhppote:"offset:28"`
	}{
		MsgType:      0x58,
		Uint32:       423187757,
		Address:      net.IPv4(192, 168, 1, 2),
		MacAddress:   mac,
		SerialNumber: 423187757,
		Version:      0x0892,
		Date:         types.Date{date},
	}

	m, err := Marshal(request)

	if err != nil {
		t.Errorf("Marshal returned unexpected error: %v", err)
		return
	}

	if !reflect.DeepEqual(m, expected) {
		t.Errorf("Marshal returned invalid message - \nExpected:\n%s\nReturned:\n%s", print(expected), print(m))
		return
	}
}

func TestUnmarshal(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x92,
		0x20, 0x18, 0x08, 0x16, 0x20, 0x18, 0x12, 0x31, 0x12, 0x23, 0x34, 0x01, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := struct {
		MsgType      byte               `uhppote:"offset:1"`
		Uint32       uint32             `uhppote:"offset:4"`
		Address      net.IP             `uhppote:"offset:8"`
		SubnetMask   net.IP             `uhppote:"offset:12"`
		Gateway      net.IP             `uhppote:"offset:16"`
		MacAddress   net.HardwareAddr   `uhppote:"offset:20"`
		SerialNumber types.SerialNumber `uhppote:"offset:26"`
		Version      types.Version      `uhppote:"offset:30"`
		Date         types.Date         `uhppote:"offset:32"`
		DateTime     types.DateTime     `uhppote:"offset:36"`
		True         bool               `uhppote:"offset:43"`
		False        bool               `uhppote:"offset:44"`
	}{}

	err := Unmarshal(message, &reply)

	if err != nil {
		t.Errorf("Unexpected error: %v", err)
		return
	}

	if reply.MsgType != 0x94 {
		t.Errorf("Expected 'byte':0x%02X, got: 0x%02X\n", 0x94, reply.MsgType)
	}

	if reply.Uint32 != 423187757 {
		t.Errorf("Expected 'uint32':%v, got: %v\n", 423187757, reply.Uint32)
	}

	if !reflect.DeepEqual(reply.Address, net.IPv4(192, 168, 0, 0)) {
		t.Errorf("Expected IP address '%v', got: '%v'\n", net.IPv4(192, 168, 0, 0), reply.Address)
	}

	if !reflect.DeepEqual(reply.SubnetMask, net.IPv4(255, 255, 255, 0)) {
		t.Errorf("Expected subnet mask '%v', got: '%v'\n", net.IPv4(255, 255, 255, 0), reply.SubnetMask)
	}

	if !reflect.DeepEqual(reply.Gateway, net.IPv4(0, 0, 0, 0)) {
		t.Errorf("Expected subnet mask '%v', got: '%v'\n", net.IPv4(0, 0, 0, 0), reply.Gateway)
	}

	MAC, _ := net.ParseMAC("00:66:19:39:55:2d")
	if !reflect.DeepEqual(reply.MacAddress, MAC) {
		t.Errorf("Expected MAC address '%v', got: '%v'\n", MAC, reply.MacAddress)
	}

	if reply.Version != 0x0892 {
		t.Errorf("Expected version '0x%04X', got: '0x%04X'\n", 0x0892, reply.Version)
	}

	date, _ := time.ParseInLocation("2006-01-02", "2018-08-16", time.Local)
	if reply.Date.Date != date {
		t.Errorf("Expected date '%v', got: '%v'\n", date, reply.Date)
	}

	datetime, _ := time.ParseInLocation("2006-01-02 15:04:05", "2018-12-31 12:23:34", time.Local)
	if reply.DateTime.DateTime != datetime {
		t.Errorf("Expected date '%v', got: '%v'\n", datetime, reply.DateTime)
	}

	if reply.True != true {
		t.Errorf("Expected door 1 '%v', got: '%v\n", true, reply.True)
	}

	if reply.False != false {
		t.Errorf("Expected door 2 '%v', got: '%v\n", false, reply.False)
	}
}

func print(m []byte) string {
	regex := regexp.MustCompile("(?m)^(.*)")

	return fmt.Sprintf("%s", regex.ReplaceAllString(hex.Dump(m), "$1"))
}
