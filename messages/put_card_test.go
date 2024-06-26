package messages

import (
	"reflect"
	"testing"
	"time"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/types"
)

func TestMarshalPutCardRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x50, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xac, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x01, 0x02,
		0x20, 0x19, 0x12, 0x31, 0x01, 0x00, 0x1d, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	from, _ := time.ParseInLocation("2006-01-02", "2019-01-02", time.Local)
	to, _ := time.ParseInLocation("2006-01-02", "2019-12-31", time.Local)
	request := PutCardRequest{
		SerialNumber: 423187757,
		CardNumber:   6154412,
		From:         types.Date(from),
		To:           types.Date(to),
		Door1:        1,
		Door2:        0,
		Door3:        29,
		Door4:        1,
	}

	m, err := codec.Marshal(request)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", codec.Dump(expected, ""), codec.Dump(m, ""))
	}
}

func TestMarshalPutCardRequestWithPIN(t *testing.T) {
	expected := []byte{
		0x17, 0x50, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xac, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x01, 0x02,
		0x20, 0x19, 0x12, 0x31, 0x01, 0x00, 0x1d, 0x01, 0xd7, 0xe1, 0x0e, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	from, _ := time.ParseInLocation("2006-01-02", "2019-01-02", time.Local)
	to, _ := time.ParseInLocation("2006-01-02", "2019-12-31", time.Local)
	request := PutCardRequest{
		SerialNumber: 423187757,
		CardNumber:   6154412,
		From:         types.Date(from),
		To:           types.Date(to),
		Door1:        1,
		Door2:        0,
		Door3:        29,
		Door4:        1,
		PIN:          975319,
	}

	m, err := codec.Marshal(request)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", codec.Dump(expected, ""), codec.Dump(m, ""))
	}
}

func TestFactoryUnmarshalPutCardRequest(t *testing.T) {
	message := []byte{
		0x17, 0x50, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xac, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x01, 0x02,
		0x20, 0x19, 0x12, 0x31, 0x01, 0x00, 29, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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

	rq, ok := request.(*PutCardRequest)
	if !ok {
		t.Fatalf("Invalid request type - expected:%T, got: %T", &PutCardRequest{}, request)
	}

	if rq.MsgType != 0x50 {
		t.Errorf("Incorrect 'message type' from valid message: %02x", rq.MsgType)
	}

	if rq.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' from valid message: %v", rq.SerialNumber)
	}

	if rq.CardNumber != 6154412 {
		t.Errorf("Incorrect 'card number' from valid message: %v", rq.CardNumber)
	}

	from, _ := time.ParseInLocation("2006-01-02", "2019-01-02", time.Local)
	if rq.From != types.Date(from) {
		t.Errorf("Incorrect 'from date' from valid message: %v", rq.From)
	}

	to, _ := time.ParseInLocation("2006-01-02", "2019-12-31", time.Local)
	if rq.To != types.Date(to) {
		t.Errorf("Incorrect 'to date' from valid message: %v", rq.To)
	}

	if rq.Door1 != 1 {
		t.Errorf("Incorrect 'door 1' from valid message: %v", rq.Door1)
	}

	if rq.Door2 != 0 {
		t.Errorf("Incorrect 'door 2' from valid message: %v", rq.Door2)
	}

	if rq.Door3 != 29 {
		t.Errorf("Incorrect 'door 3' from valid message: %v", rq.Door3)
	}

	if rq.Door4 != 1 {
		t.Errorf("Incorrect 'door 4' from valid message: %v", rq.Door4)
	}

	if rq.PIN != 0 {
		t.Errorf("Incorrect 'PIN' from valid message: %v", rq.PIN)
	}
}

func TestFactoryUnmarshalPutCardRequestWithPIN(t *testing.T) {
	message := []byte{
		0x17, 0x50, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xac, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x01, 0x02,
		0x20, 0x19, 0x12, 0x31, 0x01, 0x00, 0x1d, 0x01, 0xd1, 0x2f, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00,
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

	rq, ok := request.(*PutCardRequest)
	if !ok {
		t.Fatalf("Invalid request type - expected:%T, got: %T", &PutCardRequest{}, request)
	}

	if rq.MsgType != 0x50 {
		t.Errorf("Incorrect 'message type' from valid message: %02x", rq.MsgType)
	}

	if rq.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' from valid message: %v", rq.SerialNumber)
	}

	if rq.CardNumber != 6154412 {
		t.Errorf("Incorrect 'card number' from valid message: %v", rq.CardNumber)
	}

	from, _ := time.ParseInLocation("2006-01-02", "2019-01-02", time.Local)
	if rq.From != types.Date(from) {
		t.Errorf("Incorrect 'from date' from valid message: %v", rq.From)
	}

	to, _ := time.ParseInLocation("2006-01-02", "2019-12-31", time.Local)
	if rq.To != types.Date(to) {
		t.Errorf("Incorrect 'to date' from valid message: %v", rq.To)
	}

	if rq.Door1 != 1 {
		t.Errorf("Incorrect 'door 1' from valid message: %v", rq.Door1)
	}

	if rq.Door2 != 0 {
		t.Errorf("Incorrect 'door 2' from valid message: %v", rq.Door2)
	}

	if rq.Door3 != 29 {
		t.Errorf("Incorrect 'door 3' from valid message: %v", rq.Door3)
	}

	if rq.Door4 != 1 {
		t.Errorf("Incorrect 'door 4' from valid message: %v", rq.Door4)
	}

	if rq.PIN != 864209 {
		t.Errorf("Incorrect 'PIN' from valid message: %v", rq.PIN)
	}
}

func TestUnmarshalPutCardResponse(t *testing.T) {
	message := []byte{
		0x17, 0x50, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := PutCardResponse{}

	err := codec.Unmarshal(message, &reply)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x50 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x\n", 0x92, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v\n", 423187757, reply.SerialNumber)
	}
}

func TestFactoryUnmarshalPutCardResponse(t *testing.T) {
	message := []byte{
		0x17, 0x50, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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

	reply, ok := response.(*PutCardResponse)
	if !ok {
		t.Fatalf("Invalid response type - expected:%T, got: %T\n", &PutCardResponse{}, response)
	}

	if reply.MsgType != 0x50 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x\n", 0x92, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v\n", 423187757, reply.SerialNumber)
	}
}

func TestUnmarshalPutCardResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := PutCardResponse{}

	err := codec.Unmarshal(message, &reply)
	if err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0x50, received 0x94")
	}
}
