package messages

import (
	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/types"
	"net"
	"reflect"
	"testing"
	"time"
)

func TestMarshalGetDeviceRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x94, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := GetDeviceRequest{}

	m, err := codec.Marshal(request)

	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array for uhppote.Marshal(%s):\nExpected:\n%s\nReturned:\n%s", "GetDeviceRequest", codec.Dump(expected, ""), codec.Dump(m, ""))
	}
}

func TestUnmarshalGetDeviceResponse(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x08, 0x92, 0x20, 0x18, 0x08, 0x16,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetDeviceResponse{}

	err := codec.Unmarshal(message, &reply)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x94 {
		t.Errorf("Incorrect 'message type' from valid message: %02x\n", reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' from valid message: %v\n", reply.SerialNumber)
	}

	if !reflect.DeepEqual(reply.IpAddress, net.IPv4(192, 168, 0, 0)) {
		t.Errorf("Incorrect 'IP address' from valid message: %v\n", reply.IpAddress)
	}

	if !reflect.DeepEqual(reply.SubnetMask, net.IPv4(255, 255, 255, 0)) {
		t.Errorf("Incorrect 'subnet mask' from valid message: %v\n", reply.SubnetMask)
	}

	if !reflect.DeepEqual(reply.Gateway, net.IPv4(0, 0, 0, 0)) {
		t.Errorf("Incorrect 'gateway' from valid message: %v\n", reply.Gateway)
	}

	MAC, _ := net.ParseMAC("00:66:19:39:55:2d")
	if !reflect.DeepEqual(reply.MacAddress, types.MacAddress(MAC)) {
		t.Errorf("Incorrect 'MAC address' - expected:'%v', got:'%v'\n", types.MacAddress(MAC), reply.MacAddress)
	}

	if reply.Version != 0x0892 {
		t.Errorf("Incorrect 'version' from valid message: %v\n", reply.Version)
	}

	date, _ := time.ParseInLocation("20060102", "20180816", time.Local)
	if reply.Date != types.Date(date) {
		t.Errorf("Incorrect 'date' from valid message: %v\n", reply.Date)
	}
}

func TestFactoryUnmarshalGetDeviceResponse(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x08, 0x92, 0x20, 0x18, 0x08, 0x16,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	response, err := UnmarshalResponse(message)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if response == nil {
		t.Fatalf("Unexpected response: %v\n", response)
	}

	reply, ok := response.(*GetDeviceResponse)
	if !ok {
		t.Fatalf("Invalid response type - expected:%T, got: %T\n", &GetDeviceResponse{}, response)
	}

	if reply.MsgType != 0x94 {
		t.Errorf("Incorrect 'message type' from valid message: %02x\n", reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' from valid message: %v\n", reply.SerialNumber)
	}

	if !reflect.DeepEqual(reply.IpAddress, net.IPv4(192, 168, 0, 0)) {
		t.Errorf("Incorrect 'IP address' from valid message: %v\n", reply.IpAddress)
	}

	if !reflect.DeepEqual(reply.SubnetMask, net.IPv4(255, 255, 255, 0)) {
		t.Errorf("Incorrect 'subnet mask' from valid message: %v\n", reply.SubnetMask)
	}

	if !reflect.DeepEqual(reply.Gateway, net.IPv4(0, 0, 0, 0)) {
		t.Errorf("Incorrect 'gateway' from valid message: %v\n", reply.Gateway)
	}

	MAC, _ := net.ParseMAC("00:66:19:39:55:2d")
	if !reflect.DeepEqual(reply.MacAddress, types.MacAddress(MAC)) {
		t.Errorf("Incorrect 'MAC address' - expected:'%v', got:'%v'\n", types.MacAddress(MAC), reply.MacAddress)
	}

	if reply.Version != 0x0892 {
		t.Errorf("Incorrect 'version' from valid message: %v\n", reply.Version)
	}

	date, _ := time.ParseInLocation("20060102", "20180816", time.Local)
	if reply.Date != types.Date(date) {
		t.Errorf("Incorrect 'date' from valid message: %v\n", reply.Date)
	}
}

func TestUnmarshalGetDeviceResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x92, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x08, 0x92, 0x20, 0x18, 0x08, 0x16,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetDeviceResponse{}

	err := codec.Unmarshal(message, &reply)

	if err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0x94, received 0x92")
	}
}
