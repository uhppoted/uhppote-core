package messages

import (
	"reflect"
	"testing"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
)

func TestMarshalRecordSpecialEventsRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x8e, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := RecordSpecialEventsRequest{
		SerialNumber: 423187757,
		Enable:       true,
	}

	m, err := codec.Marshal(request)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if !reflect.DeepEqual(m, expected) {
		t.Fatalf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", codec.Dump(expected, ""), codec.Dump(m, ""))
	}
}

func TestFactoryUnmarshalRecordSpecialEventsRequest(t *testing.T) {
	message := []byte{
		0x17, 0x8e, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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

	rq, ok := request.(*RecordSpecialEventsRequest)
	if !ok {
		t.Fatalf("Invalid request type - expected:%T, got: %T\n", &RecordSpecialEventsRequest{}, request)
	}

	if rq.MsgType != 0x8e {
		t.Errorf("Incorrect 'message type' from valid message: %02x\n", rq.MsgType)
	}
}

func TestUnmarshalRecordSpecialEventsResponse(t *testing.T) {
	message := []byte{
		0x17, 0x8e, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := RecordSpecialEventsResponse{}

	err := codec.Unmarshal(message, &reply)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x8e {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x8e, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if !reply.Succeeded {
		t.Errorf("Incorrect 'result' - expected:%v, got:%v", reply, reply.Succeeded)
	}
}

func TestFactoryUnmarshalRecordSpecialEventsResponse(t *testing.T) {
	message := []byte{
		0x17, 0x8e, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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

	reply, ok := response.(*RecordSpecialEventsResponse)
	if !ok {
		t.Fatalf("Invalid response type - expected:%T, got: %T\n", &RecordSpecialEventsResponse{}, response)
	}

	if reply.MsgType != 0x8e {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x8e, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if !reply.Succeeded {
		t.Errorf("Incorrect 'result' - expected:%v, got:%v", reply, reply.Succeeded)
	}
}

func TestUnmarshalRecordSpecialEventsResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := RecordSpecialEventsResponse{}

	err := codec.Unmarshal(message, &reply)
	if err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0x52, received 0x94")
	}
}
