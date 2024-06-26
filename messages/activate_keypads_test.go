package messages

import (
	"reflect"
	"testing"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
)

func TestMarshalActivateAccessKeypadsRequest(t *testing.T) {
	expected := []byte{
		0x17, 0xA4, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := ActivateAccessKeypadsRequest{
		SerialNumber: 405419896,
		Reader1:      true,
		Reader2:      true,
		Reader3:      false,
		Reader4:      true,
	}

	m, err := codec.Marshal(request)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", codec.Dump(expected, ""), codec.Dump(m, ""))
	}
}

func TestFactoryUnmarshalActivateAccessKeypadsRequest(t *testing.T) {
	message := []byte{
		0x17, 0xA4, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x020,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request, err := UnmarshalRequest(message)

	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if request == nil {
		t.Fatalf("Unexpected request: %v", request)
	}

	rq, ok := request.(*ActivateAccessKeypadsRequest)
	if !ok {
		t.Fatalf("Invalid request type - expected:%T, got: %T", &ActivateAccessKeypadsRequest{}, request)
	}

	if rq.MsgType != 0xA4 {
		t.Errorf("Incorrect 'message type' from valid message: %02x", rq.MsgType)
	}

	if rq.SerialNumber != 405419896 {
		t.Errorf("Incorrect 'serial number' from valid message: %v", rq.SerialNumber)
	}

	if rq.Reader1 != true {
		t.Errorf("Incorrect 'reader 1 activate keypad' from valid message: %v", rq.Reader1)
	}

	if rq.Reader2 != false {
		t.Errorf("Incorrect 'reader 2 activate keypad' from valid message: %v", rq.Reader2)
	}

	if rq.Reader3 != false {
		t.Errorf("Incorrect 'reader 3 activate keypad' from valid message: %v", rq.Reader3)
	}

	if rq.Reader4 != true {
		t.Errorf("Incorrect 'reader 4 activate keypad' from valid message: %v", rq.Reader4)
	}
}

func TestUnmarshalActivateAccessKeypadsResponse(t *testing.T) {
	message := []byte{
		0x17, 0xA4, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := ActivateAccessKeypadsResponse{}

	err := codec.Unmarshal(message, &reply)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0xA4 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x\n", 0xA4, reply.MsgType)
	}

	if reply.SerialNumber != 405419896 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v\n", 405419896, reply.SerialNumber)
	}

	if !reply.Succeeded {
		t.Errorf("Incorrect 'succeeded' - expected:%v, got:%v\n", true, reply.Succeeded)
	}
}

func TestFactoryUnmarshalActivateKeypadsResponse(t *testing.T) {
	message := []byte{
		0x17, 0xA4, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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

	reply, ok := response.(*ActivateAccessKeypadsResponse)
	if !ok {
		t.Fatalf("Invalid response type - expected:%T, got: %T\n", &ActivateAccessKeypadsResponse{}, response)
	}

	if reply.MsgType != 0xA4 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x\n", 0x92, reply.MsgType)
	}

	if reply.SerialNumber != 405419896 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v\n", 405419896, reply.SerialNumber)
	}

	if !reply.Succeeded {
		t.Errorf("Incorrect 'succeeded' - expected:%v, got:%v\n", true, reply.Succeeded)
	}
}

func TestUnmarshalActivateKeypadsResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := ActivateAccessKeypadsResponse{}

	err := codec.Unmarshal(message, &reply)
	if err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0xA4, received 0x94")
	}
}
