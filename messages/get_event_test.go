package messages

import (
	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/types"
	"reflect"
	"testing"
	"time"
)

func TestMarshalGetEventRequest(t *testing.T) {
	expected := []byte{
		0x17, 0xb0, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := GetEventRequest{
		SerialNumber: 423187757,
		Index:        1,
	}

	m, err := codec.Marshal(request)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", dump(expected, ""), dump(m, ""))
	}
}

func TestFactoryUnmarshalGetEventRequest(t *testing.T) {
	message := []byte{
		0x17, 0xb0, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := GetEventRequest{
		MsgType:      0xb0,
		SerialNumber: 423187757,
		Index:        1,
	}

	request, err := UnmarshalRequest(message)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	rq, ok := request.(*GetEventRequest)
	if !ok {
		t.Fatalf("Invalid request type - expected:%T, got: %T\n", &GetEventRequest{}, request)
	}

	if !reflect.DeepEqual(*rq, expected) {
		t.Errorf("Invalid unmarshalled request:\nexpected:%#v\ngot:     %#v", expected, *rq)
	}
}

func TestUnmarshalGetEventResponse(t *testing.T) {
	message := []byte{
		0x17, 0xb0, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
		0xad, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x02, 0x10, 0x07, 0x12, 0x01, 0x06, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4a, 0x26, 0x80, 0x39, 0x08, 0x92, 0x00, 0x00,
	}

	reply := GetEventResponse{}

	err := codec.Unmarshal(message, &reply)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0xb0 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x\n", 0xb0, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%d, got: %v\n", 423187757, reply.SerialNumber)
	}

	if reply.Index != 8 {
		t.Errorf("Incorrect 'index' - expected:%d, got:%d\n", 8, reply.Index)
	}

	if reply.Type != 2 {
		t.Errorf("Incorrect 'type' - expected:%d, got:%d\n", 2, reply.Type)
	}

	if reply.Granted != true {
		t.Errorf("Incorrect 'granted' - expected:%v, got:%v\n", true, reply.Granted)
	}

	if reply.Door != 3 {
		t.Errorf("Incorrect 'door' - expected:%d, got:%d\n", 3, reply.Door)
	}

	if reply.Direction != 0x01 {
		t.Errorf("Incorrect 'direction' - expected:%v, got:%v\n", 0x01, reply.Direction)
	}

	if reply.CardNumber != 6154413 {
		t.Errorf("Incorrect 'card number' - expected:%d, got: %v\n", 6154413, reply.CardNumber)
	}

	if reply.Reason != 6 {
		t.Errorf("Incorrect 'reason' - expected:%d, got: %v\n", 6, reply.Reason)
	}

	timestamp, _ := time.ParseInLocation("2006-01-02 15:04:05", "2019-02-10 07:12:01", time.Local)
	if reply.Timestamp != types.DateTime(timestamp) {
		t.Errorf("Incorrect 'timestamp' - expected:%s, got:%s\n", timestamp.Format("2006-01-02 15:04:05"), reply.Timestamp)
	}
}

func TestUnmarshalGetEventResponseWithNoEvents(t *testing.T) {
	message := []byte{
		0x17, 0xb0, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := GetEventResponse{
		MsgType:      0xb0,
		SerialNumber: 423187757,
		Index:        0,
		Timestamp:    types.DateTime{},
	}

	response := GetEventResponse{}

	err := codec.Unmarshal(message, &response)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if !reflect.DeepEqual(response, expected) {
		t.Errorf("Invalid unmarshalled response:\nexpected:%#v\ngot:     %#v", expected, response)
	}
}

func TestFactoryUnmarshalGetEventResponse(t *testing.T) {
	message := []byte{
		0x17, 0xb0, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
		0xad, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x02, 0x10, 0x07, 0x12, 0x01, 0x06, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4a, 0x26, 0x80, 0x39, 0x08, 0x92, 0x00, 0x00,
	}

	datetime, _ := time.ParseInLocation("2006-01-02 15:04:05", "2019-02-10 07:12:01", time.Local)
	timestamp := types.DateTime(datetime)
	expected := GetEventResponse{
		MsgType:      0xb0,
		SerialNumber: 423187757,
		Index:        8,
		Type:         2,
		Granted:      true,
		Door:         3,
		Direction:    0x01,
		CardNumber:   6154413,
		Timestamp:    timestamp,
		Reason:       0x06,
	}

	response, err := UnmarshalResponse(message)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	reply, ok := response.(*GetEventResponse)
	if !ok {
		t.Fatalf("Invalid response type - expected:%T, got: %T\n", &GetEventResponse{}, reply)
	}

	if !reflect.DeepEqual(*reply, expected) {
		t.Errorf("Invalid unmarshalled response:\nexpected:%#v\ngot:     %#v", expected, *reply)
	}
}

func TestUnmarshalGetEventResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
		0xad, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x02, 0x10, 0x07, 0x12, 0x01, 0x06, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4a, 0x26, 0x80, 0x39, 0x08, 0x92, 0x00, 0x00,
	}

	reply := GetEventResponse{}

	err := codec.Unmarshal(message, &reply)
	if err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0xb0, received 0x94")
	}
}

func TestUnmarshalGetEventResponseWithInvalidTimestamp(t *testing.T) {
	message := []byte{
		0x17, 0xb0, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xac, 0x2b, 0x03, 0x00, 0x01, 0x00, 0x02, 0x01,
		0xa0, 0x7a, 0x99, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x91, 0x00, 0x00,
	}

	reply := GetEventResponse{}

	err := codec.Unmarshal(message, &reply)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0xb0 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x\n", 0xb0, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%d, got: %v\n", 423187757, reply.SerialNumber)
	}

	if reply.Index != 207788 {
		t.Errorf("Incorrect 'index' - expected:%d, got:%d\n", 207788, reply.Index)
	}

	if reply.Type != 1 {
		t.Errorf("Incorrect 'type' - expected:%d, got:%d\n", 1, reply.Type)
	}

	if reply.Granted != false {
		t.Errorf("Incorrect 'granted' - expected:%v, got:%v\n", false, reply.Granted)
	}

	if reply.Door != 2 {
		t.Errorf("Incorrect 'door' - expected:%d, got:%d\n", 2, reply.Door)
	}

	if reply.Direction != 0x01 {
		t.Errorf("Incorrect 'direction' - expected:%v, got:%v\n", 0x01, reply.Direction)
	}

	if reply.CardNumber != 10058400 {
		t.Errorf("Incorrect 'card number' - expected:%d, got: %v\n", 10058400, reply.CardNumber)
	}

	if reply.Reason != 6 {
		t.Errorf("Incorrect 'reason' - expected:%d, got: %v\n", 6, reply.Reason)
	}

	if !reply.Timestamp.IsZero() {
		t.Errorf("Incorrect 'timestamp' - expected:%s, got:%s\n", "", reply.Timestamp)
	}
}
