package messages

import (
	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/types"
	"reflect"
	"testing"
	"time"
)

func TestMarshalGetCardByIndexRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x5c, 0x00, 0x00, 0x2D, 0x55, 0x39, 0x19, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := GetCardByIndexRequest{
		SerialNumber: 423187757,
		Index:        4,
	}

	if m, err := codec.Marshal(request); err != nil {
		t.Fatalf("Unexpected error: %v", err)
	} else if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", dump(expected, ""), dump(m, ""))
	}
}

func TestMarshalGetCardByIdRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x5a, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xac, 0xe8, 0x5d, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := GetCardByIDRequest{
		SerialNumber: 423187757,
		CardNumber:   6154412,
	}

	if m, err := codec.Marshal(request); err != nil {
		t.Fatalf("Unexpected error: %v", err)
	} else if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", dump(expected, ""), dump(m, ""))
	}
}

func TestUnmarshalGetCardByIndexResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5c, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xac, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x02, 0x03,
		0x20, 0x19, 0x12, 0x29, 0x00, 0x00, 29, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetCardByIndexResponse{}

	if err := codec.Unmarshal(message, &reply); err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x5C {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x5C, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if reply.CardNumber != 6154412 {
		t.Errorf("Incorrect 'card number' - expected:%v, got:%v", 6154412, reply.CardNumber)
	}

	from := types.ToDate(2019, time.February, 3)
	if reply.From != from {
		t.Errorf("Incorrect 'from date' - expected:%v, got:%v", from, reply.From)
	}

	to := types.ToDate(2019, time.December, 29)
	if reply.To != types.Date(to) {
		t.Errorf("Incorrect 'to date' - expected:%v, got:%v", to, reply.To)
	}

	if reply.Door1 != 0 {
		t.Errorf("Incorrect 'door 1' - expected:%v, got:%v", false, reply.Door1)
	}

	if reply.Door2 != 0 {
		t.Errorf("Incorrect 'door 2' - expected:%v, got:%v", false, reply.Door2)
	}

	if reply.Door3 != 29 {
		t.Errorf("Incorrect 'door 3' - expected:%v, got:%v", true, reply.Door3)
	}

	if reply.Door4 != 1 {
		t.Errorf("Incorrect 'door 4' - expected:%v, got:%v", true, reply.Door4)
	}

	if reply.PIN != 0 {
		t.Errorf("Incorrect 'PIN' - expected:%v, got:%v", 0, reply.PIN)
	}
}

func TestUnmarshalGetCardWithPINByIndexResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5c, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xac, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x02, 0x03,
		0x20, 0x19, 0x12, 0x29, 0x00, 0x00, 0x1d, 0x01, 0xa4, 0x0d, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetCardByIndexResponse{}

	if err := codec.Unmarshal(message, &reply); err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x5C {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x5C, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if reply.CardNumber != 6154412 {
		t.Errorf("Incorrect 'card number' - expected:%v, got:%v", 6154412, reply.CardNumber)
	}

	from := types.ToDate(2019, time.February, 3)
	if reply.From != from {
		t.Errorf("Incorrect 'from date' - expected:%v, got:%v", from, reply.From)
	}

	to := types.ToDate(2019, time.December, 29)
	if reply.To != to {
		t.Errorf("Incorrect 'to date' - expected:%v, got:%v", to, reply.To)
	}

	if reply.Door1 != 0 {
		t.Errorf("Incorrect 'door 1' - expected:%v, got:%v", false, reply.Door1)
	}

	if reply.Door2 != 0 {
		t.Errorf("Incorrect 'door 2' - expected:%v, got:%v", false, reply.Door2)
	}

	if reply.Door3 != 29 {
		t.Errorf("Incorrect 'door 3' - expected:%v, got:%v", true, reply.Door3)
	}

	if reply.Door4 != 1 {
		t.Errorf("Incorrect 'door 4' - expected:%v, got:%v", true, reply.Door4)
	}

	if reply.PIN != 986532 {
		t.Errorf("Incorrect 'PIN' - expected:%v, got:%v", 0, reply.PIN)
	}
}

func TestUnmarshalGetCardByIndexNotFoundResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5c, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetCardByIndexResponse{}

	if err := codec.Unmarshal(message, &reply); err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x5C {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x5C, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if reply.CardNumber != 0 {
		t.Errorf("Incorrect 'card number' - expected:%v, got:%v", 6154412, reply.CardNumber)
	}

	if !reply.From.IsZero() {
		t.Errorf("Incorrect 'from date' - expected:%v, got:%v", types.Date{}, reply.From)
	}

	if !reply.To.IsZero() {
		t.Errorf("Incorrect 'to date' - expected:%v, got:%v", types.Date{}, reply.To)
	}

	if reply.Door1 != 0 {
		t.Errorf("Incorrect 'door 1' - expected:%v, got:%v", false, reply.Door1)
	}

	if reply.Door2 != 0 {
		t.Errorf("Incorrect 'door 2' - expected:%v, got:%v", false, reply.Door2)
	}

	if reply.Door3 != 0 {
		t.Errorf("Incorrect 'door 3' - expected:%v, got:%v", true, reply.Door3)
	}

	if reply.Door4 != 0 {
		t.Errorf("Incorrect 'door 4' - expected:%v, got:%v", true, reply.Door4)
	}
}

func TestUnmarshalGetCardByIdResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5a, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xac, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x02, 0x03,
		0x20, 0x19, 0x12, 0x29, 0x00, 0x00, 0x1d, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetCardByIDResponse{}

	if err := codec.Unmarshal(message, &reply); err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x5a {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x5a, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if reply.CardNumber != 6154412 {
		t.Errorf("Incorrect 'card number' - expected:%v, got:%v", 6154412, reply.CardNumber)
	}

	from := types.ToDate(2019, time.February, 03)
	if reply.From != from {
		t.Errorf("Incorrect 'from date' - expected:%v, got:%v", from, reply.From)
	}

	to := types.ToDate(2019, time.December, 29)
	if reply.To != to {
		t.Errorf("Incorrect 'to date' - expected:%v, got:%v", to, reply.To)
	}

	if reply.Door1 != 0 {
		t.Errorf("Incorrect 'door 1' - expected:%v, got:%v", false, reply.Door1)
	}

	if reply.Door2 != 0 {
		t.Errorf("Incorrect 'door 2' - expected:%v, got:%v", false, reply.Door2)
	}

	if reply.Door3 != 29 {
		t.Errorf("Incorrect 'door 3' - expected:%v, got:%v", true, reply.Door3)
	}

	if reply.Door4 != 1 {
		t.Errorf("Incorrect 'door 4' - expected:%v, got:%v", true, reply.Door4)
	}

	if reply.PIN != 0 {
		t.Errorf("Incorrect 'PIN' - expected:%v, got:%v", 0, reply.PIN)
	}
}

func TestUnmarshalGetCardWithPINByIdResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5a, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xac, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x02, 0x03,
		0x20, 0x19, 0x12, 0x29, 0x00, 0x00, 0x1d, 0x01, 0x16, 0x39, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetCardByIDResponse{}

	if err := codec.Unmarshal(message, &reply); err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x5a {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x5a, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if reply.CardNumber != 6154412 {
		t.Errorf("Incorrect 'card number' - expected:%v, got:%v", 6154412, reply.CardNumber)
	}

	from := types.ToDate(2019, time.February, 3)
	if reply.From != from {
		t.Errorf("Incorrect 'from date' - expected:%v, got:%v", from, reply.From)
	}

	to := types.ToDate(2019, time.December, 29)
	if reply.To != to {
		t.Errorf("Incorrect 'to date' - expected:%v, got:%v", to, reply.To)
	}

	if reply.Door1 != 0 {
		t.Errorf("Incorrect 'door 1' - expected:%v, got:%v", false, reply.Door1)
	}

	if reply.Door2 != 0 {
		t.Errorf("Incorrect 'door 2' - expected:%v, got:%v", false, reply.Door2)
	}

	if reply.Door3 != 29 {
		t.Errorf("Incorrect 'door 3' - expected:%v, got:%v", true, reply.Door3)
	}

	if reply.Door4 != 1 {
		t.Errorf("Incorrect 'door 4' - expected:%v, got:%v", true, reply.Door4)
	}

	if reply.PIN != 997654 {
		t.Errorf("Incorrect 'PIN' - expected:%v, got:%v", 0, reply.PIN)
	}
}

func TestUnmarshalGetCardByIdNotFoundResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5a, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetCardByIDResponse{}

	if err := codec.Unmarshal(message, &reply); err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x5a {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x5a, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if reply.CardNumber != 0 {
		t.Errorf("Incorrect 'card number' - expected:%v, got:%v", 0, reply.CardNumber)
	}

	if !reply.From.IsZero() {
		t.Errorf("Incorrect 'from date' - expected:%v, got:%v", types.Date{}, reply.From)
	}

	if !reply.To.IsZero() {
		t.Errorf("Incorrect 'to date' - expected:%v, got:%v", types.Date{}, reply.To)
	}

	if reply.Door1 != 0 {
		t.Errorf("Incorrect 'door 1' - expected:%v, got:%v", false, reply.Door1)
	}

	if reply.Door2 != 0 {
		t.Errorf("Incorrect 'door 2' - expected:%v, got:%v", false, reply.Door2)
	}

	if reply.Door3 != 0 {
		t.Errorf("Incorrect 'door 3' - expected:%v, got:%v", true, reply.Door3)
	}

	if reply.Door4 != 0 {
		t.Errorf("Incorrect 'door 4' - expected:%v, got:%v", true, reply.Door4)
	}
}

func TestUnmarshalGetCardByIndexResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x92,
		0x20, 0x18, 0x08, 0x16, 0x20, 0x18, 0x12, 0x31, 0x12, 0x23, 0x34, 0x01, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetCardByIndexResponse{}

	if err := codec.Unmarshal(message, &reply); err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0x5c, received 0x94")
	}
}

func TestUnmarshalGetCardByIdResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x92,
		0x20, 0x18, 0x08, 0x16, 0x20, 0x18, 0x12, 0x31, 0x12, 0x23, 0x34, 0x01, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetCardByIDResponse{}

	if err := codec.Unmarshal(message, &reply); err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0x5a, received 0x94")
	}
}
