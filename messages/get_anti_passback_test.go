package messages

import (
	"reflect"
	"testing"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
)

func TestMarshalGetAntiPassbackRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x86, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := GetAntiPassbackRequest{
		SerialNumber: 405419896,
	}

	m, err := codec.Marshal(request)

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	} else if !reflect.DeepEqual(m, expected) {
		t.Errorf("invalid marshalled byte array\nexpected:\n%s\ngot:\n%s", codec.Dump(expected, ""), codec.Dump(m, ""))
	}
}

func TestUnmarshalGetAntiPassbackResponse(t *testing.T) {
	message := []byte{
		0x17, 0x86, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := GetAntiPassbackResponse{
		MsgType:      0x86,
		SerialNumber: 405419896,
		AntiPassback: 0x04,
	}

	reply := GetAntiPassbackResponse{}

	if err := codec.Unmarshal(message, &reply); err != nil {
		t.Fatalf("unexpected error: %v\n", err)
	} else if !reflect.DeepEqual(reply, expected) {
		t.Errorf("incorrectly unmarshalled response\n   expected:%v\n   got:     \n%v", expected, reply)
	}
}

func TestFactoryUnmarshalGetAntiPassbackResponse(t *testing.T) {
	message := []byte{
		0x17, 0x86, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := GetAntiPassbackResponse{
		MsgType:      0x86,
		SerialNumber: 405419896,
		AntiPassback: 0x04,
	}

	if response, err := UnmarshalResponse(message); err != nil {
		t.Fatalf("unexpected error: %v\n", err)
	} else if response == nil {
		t.Fatalf("unexpected response: %v\n", response)
	} else if reply, ok := response.(*GetAntiPassbackResponse); !ok {
		t.Fatalf("invalid response type - expected:%T, got: %T\n", &GetAntiPassbackResponse{}, response)
	} else if reply == nil {
		t.Fatalf("invalid reply - expected:%T, got: %v\n", &GetAntiPassbackResponse{}, reply)
	} else if !reflect.DeepEqual(*reply, expected) {
		t.Errorf("incorrectly unmarshalled response\n   expected:%v\n   got:     \n%v", expected, reply)
	}
}

func TestUnmarshalGetAntiPassbackResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x92, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x08, 0x92, 0x20, 0x18, 0x08, 0x16,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetAntiPassbackResponse{}

	if err := codec.Unmarshal(message, &reply); err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0x86, received 0x92")
	}
}
