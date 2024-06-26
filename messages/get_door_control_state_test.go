package messages

import (
	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"reflect"
	"testing"
)

func TestMarshalGetDoorControlStateRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x82, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := GetDoorControlStateRequest{
		SerialNumber: 423187757,
		Door:         4,
	}

	m, err := codec.Marshal(request)

	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", codec.Dump(expected, ""), codec.Dump(m, ""))
	}
}

func TestFactoryUnmarshalGetDoorControlStateRequest(t *testing.T) {
	message := []byte{
		0x17, 0x82, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request, err := UnmarshalRequest(message)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	} else if request == nil {
		t.Fatalf("Unexpected request: %v\n", request)
	}

	rq, ok := request.(*GetDoorControlStateRequest)
	if !ok {
		t.Fatalf("Invalid request type - expected:%T, got: %T\n", &GetDoorControlStateRequest{}, request)
	}

	if rq.MsgType != 0x82 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x\n", 0x82, rq.MsgType)
	}

	if rq.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v\n", 423187757, rq.SerialNumber)
	}

	if rq.Door != 4 {
		t.Errorf("Incorrect 'door' - expected:%v, got:%v\n", 4, rq.Door)
	}
}

func TestUnmarshalGetDoorControlStateResponse(t *testing.T) {
	message := []byte{
		0x17, 0x82, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x04, 0x02, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetDoorControlStateResponse{}

	err := codec.Unmarshal(message, &reply)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x82 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x\n", 0x82, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v\n", 423187757, reply.SerialNumber)
	}

	if reply.Door != 4 {
		t.Errorf("Incorrect 'door' - expected:%v, got:%v\n", 4, reply.Door)
	}

	if reply.ControlState != 2 {
		t.Errorf("Incorrect 'control state' - expected:%v, got:%v\n", 2, reply.ControlState)
	}

	if reply.Delay != 5 {
		t.Errorf("Incorrect 'delay' - expected:%v, got:%v\n", 5, reply.Delay)
	}
}

func TestFactoryUnmarshalGetDoorControlStateResponse(t *testing.T) {
	message := []byte{
		0x17, 0x82, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x04, 0x02, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	response, err := UnmarshalResponse(message)

	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	} else if response == nil {
		t.Fatalf("Unexpected response: %v\n", response)
	}

	reply, ok := response.(*GetDoorControlStateResponse)
	if !ok {
		t.Fatalf("Invalid response type - expected:%T, got: %T\n", &GetDoorControlStateResponse{}, response)
	}

	if reply.MsgType != 0x82 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x\n", 0x82, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v\n", 423187757, reply.SerialNumber)
	}

	if reply.Door != 4 {
		t.Errorf("Incorrect 'door' - expected:%v, got:%v\n", 4, reply.Door)
	}

	if reply.ControlState != 2 {
		t.Errorf("Incorrect 'control state' - expected:%v, got:%v\n", 2, reply.ControlState)
	}

	if reply.Delay != 5 {
		t.Errorf("Incorrect 'delay' - expected:%v, got:%v\n", 5, reply.Delay)
	}
}

func TestUnmarshalGetDoorControlStateResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x04, 0x03, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetDoorControlStateResponse{}

	err := codec.Unmarshal(message, &reply)
	if err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0x82, received 0x94")
	}
}
