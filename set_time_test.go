package uhppote

import (
	"reflect"
	"testing"
	"time"
	codec "uhppote/encoding/UTO311-L0x"
	"uhppote/types"
)

func TestMarshalSetTimeRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x30, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x20, 0x19, 0x04, 0x21, 0x09, 0x44, 0x40, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	datetime, _ := time.ParseInLocation("2006-01-02 15:04:05", "2019-04-21 09:44:40", time.Local)
	request := SetTimeRequest{
		SerialNumber: 423187757,
		DateTime:     types.DateTime{datetime},
	}

	m, err := codec.Marshal(request)

	if err != nil {
		t.Errorf("Unexpected error: %v", err)
		return
	}

	if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", print(expected), print(m))
		return
	}
}

func TestUnmarshalSetTimeResponse(t *testing.T) {
	message := []byte{
		0x17, 0x30, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x20, 0x19, 0x12, 0x29, 0x12, 0x34, 0x56, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := SetTimeResponse{}

	err := codec.Unmarshal(message, &reply)

	if err != nil {
		t.Errorf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x30 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x\n", 0x30, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' from valid message: %v\n", reply.SerialNumber)
	}

	datetime, _ := time.ParseInLocation("2006-01-02 15:04:05", "2019-12-29 12:34:56", time.Local)
	if reply.DateTime.DateTime != datetime {
		t.Errorf("Incorrect 'date/time' - expected:%s, got:%s\n", datetime.Format("2006-01-02 15:04:05"), reply.DateTime)
	}
}

func TestUnmarshalSetTimeResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x20, 0x19, 0x12, 0x29, 0x12, 0x34, 0x56, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := SetTimeResponse{}

	err := codec.Unmarshal(message, &reply)

	if err == nil {
		t.Errorf("Expected error: '%v'", "Invalid value in message - expected 0x30, received 0x94")
		return
	}
}
