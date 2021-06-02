package messages

import (
	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"reflect"
	"testing"
)

func TestMarshalGetCardsRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x58, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := GetCardsRequest{
		SerialNumber: 423187757,
	}

	m, err := codec.Marshal(request)

	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
		return
	}

	if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", dump(expected, ""), dump(m, ""))
	}
}

func TestFactoryUnmarshalGetCardsRequest(t *testing.T) {
	message := []byte{
		0x17, 0x58, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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

	rq, ok := request.(*GetCardsRequest)
	if !ok {
		t.Fatalf("Invalid request type - expected:%T, got: %T\n", &GetCardsRequest{}, request)
	}

	if rq.MsgType != 0x58 {
		t.Errorf("Incorrect 'message type' from valid message: %02x\n", rq.MsgType)
	}

	if rq.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, rq.SerialNumber)
	}
}

func TestUnmarshalGetCardsResponse(t *testing.T) {
	message := []byte{
		0x17, 0x58, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetCardsResponse{}

	err := codec.Unmarshal(message, &reply)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x58 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x58, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if reply.Records != 13 {
		t.Errorf("Incorrect 'records' - expected:%v, got:%v", 13, reply.Records)
	}
}

func TestFactoryUnmarshalGetCardsResponse(t *testing.T) {
	message := []byte{
		0x17, 0x58, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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

	reply, ok := response.(*GetCardsResponse)
	if !ok {
		t.Fatalf("Invalid response type - expected:%T, got: %T\n", &DeleteCardResponse{}, response)
	}

	if reply.MsgType != 0x58 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x58, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if reply.Records != 13 {
		t.Errorf("Incorrect 'records' - expected:%v, got:%v", 13, reply.Records)
	}
}

func TestUnmarshalGetCardsResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetCardsResponse{}

	err := codec.Unmarshal(message, &reply)
	if err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0x58, received 0x94")
	}
}
