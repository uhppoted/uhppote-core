package messages

import (
	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/types"
	"reflect"
	"testing"
	"time"
)

func TestMarshalGetStatusRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x20, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := GetStatusRequest{
		SerialNumber: 423187757,
	}

	m, err := codec.Marshal(request)

	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", codec.Dump(expected, ""), codec.Dump(m, ""))
	}
}

func TestFactoryUnmarshalGetStatusRequest(t *testing.T) {
	message := []byte{
		0x17, 0x20, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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

	rq, ok := request.(*GetStatusRequest)
	if !ok {
		t.Fatalf("Invalid request type - expected:%T, got: %T\n", &GetStatusRequest{}, request)
	}

	if rq.MsgType != 0x20 {
		t.Errorf("Incorrect 'message type' from valid message: %02x\n", rq.MsgType)
	}
}

func TestUnmarshalGetStatusResponse(t *testing.T) {
	message := []byte{
		0x17, 0x20, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x39, 0x00, 0x00, 0x00, 0x01, 0x00, 0x03, 0x01,
		0xaa, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x04, 0x19, 0x17, 0x00, 0x09, 0x06, 0x01, 0x00, 0x01, 0x01,
		0x00, 0x00, 0x01, 0x01, 0x09, 0x14, 0x37, 0x02, 0x11, 0x00, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00,
		0x2b, 0x04, 0x01, 0x19, 0x04, 0x20, 0x00, 0x00, 0x93, 0x26, 0x04, 0x88, 0x08, 0x92, 0x00, 0x00,
	}

	reply := GetStatusResponse{}

	err := codec.Unmarshal(message, &reply)

	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x20 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x32, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if reply.EventIndex != 57 {
		t.Errorf("Incorrect 'last index' - expected:%v, got:%v", 57, reply.EventIndex)
	}

	if reply.EventType != 1 {
		t.Errorf("Incorrect 'event type' - expected:%v, got:%v", 1, reply.EventType)
	}

	if reply.Granted {
		t.Errorf("Incorrect 'access granted' - expected:%v, got:%v", false, reply.Granted)
	}

	if reply.Door != 3 {
		t.Errorf("Incorrect 'door' - expected:%v, got:%v", 3, reply.Door)
	}

	if reply.Direction != 0x01 {
		t.Errorf("Incorrect 'direction' - expected:%v, got:%v", 0x01, reply.Direction)
	}

	if reply.CardNumber != 6154410 {
		t.Errorf("Incorrect 'card number' - expected:%v, got:%v", 6154410, reply.CardNumber)
	}

	swiped, _ := time.ParseInLocation("2006-01-02 15:04:05", "2019-04-19 17:00:09", time.Local)
	if reply.Timestamp.IsZero() || reply.Timestamp != types.DateTime(swiped) {
		t.Errorf("Incorrect 'event timestamp' - expected:%v, got:%v", swiped.Format("2006-01-02 15:04:05"), reply.Timestamp)
	}

	if reply.Reason != 6 {
		t.Errorf("Incorrect 'event reason' - expected:%v, got:%v", 6, reply.Reason)
	}

	if !reply.Door1State {
		t.Errorf("Incorrect 'door 1 state' - expected:%v, got:%v", true, reply.Door1State)
	}

	if reply.Door2State {
		t.Errorf("Incorrect 'door 2 state' - expected:%v, got:%v", false, reply.Door2State)
	}

	if !reply.Door3State {
		t.Errorf("Incorrect 'door 3 state' - expected:%v, got:%v", true, reply.Door3State)
	}

	if !reply.Door4State {
		t.Errorf("Incorrect 'door 4 state' - expected:%v, got:%v", true, reply.Door4State)
	}

	if reply.Door1Button {
		t.Errorf("Incorrect 'door 1 button' - expected:%v, got:%v", false, reply.Door1Button)
	}

	if reply.Door2Button {
		t.Errorf("Incorrect 'door 2 button' - expected:%v, got:%v", false, reply.Door2Button)
	}

	if !reply.Door3Button {
		t.Errorf("Incorrect 'door 3 button' - expected:%v, got:%v", true, reply.Door3Button)
	}

	if !reply.Door4Button {
		t.Errorf("Incorrect 'door 4 button' - expected:%v, got:%v", true, reply.Door4Button)
	}

	if reply.SystemError != 0x09 {
		t.Errorf("Incorrect 'system error' - expected:%v, got:%v", 0x09, reply.SystemError)
	}

	sysdate, _ := time.ParseInLocation("2006-01-02", "2019-04-20", time.Local)
	if reply.SystemDate != types.SystemDate(sysdate) {
		t.Errorf("Incorrect 'system date' - expected:%v, got:%v", sysdate.Format("2006-01-02"), reply.SystemDate)
	}

	systime, _ := time.ParseInLocation("15:04:05", "14:37:02", time.Local)
	if reply.SystemTime != types.SystemTime(systime) {
		t.Errorf("Incorrect 'system time' - expected:%v, got:%v", systime.Format("15:04:05"), reply.SystemTime)
	}

	if reply.SequenceId != 17 {
		t.Errorf("Incorrect 'sequence ID' - expected:%v, got:%v", 17, reply.SequenceId)
	}

	if reply.SpecialInfo != 43 {
		t.Errorf("Incorrect 'special info' - expected:%v, got:%v", 43, reply.SpecialInfo)
	}

	if reply.RelayState != 0x04 {
		t.Errorf("Incorrect 'relay state' - expected:%v, got:%v", 0x04, reply.RelayState)
	}

	if reply.InputState != 0x01 {
		t.Errorf("Incorrect 'input state' - expected:%v, got:%v", 0x01, reply.InputState)
	}
}

// Ref. https://github.com/uhppoted/uhppoted-dll/issues/7
func TestUnmarshalGetStatusResponseWithoutEvent(t *testing.T) {
	message := []byte{
		0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x83, 0x11, 0x16, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x23, 0x09, 0x29, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := GetStatusResponse{
		MsgType:      0x20,
		SerialNumber: 405419896,
		SystemError:  0x83,
		SystemDate:   mustParseSystemDate("230929"),
		SystemTime:   mustParseSystemTime("11:16:20"),
	}

	reply := GetStatusResponse{}

	err := codec.Unmarshal(message, &reply)

	if err != nil {
		t.Fatalf("unexpected error: %v\n", err)
	}

	if !reflect.DeepEqual(reply, expected) {
		t.Errorf("incorrect response:\n   expected: %#v\n   got:      %#v\n", expected, reply)
	}
}

func TestFactoryUnmarshalGetStatusResponse(t *testing.T) {
	message := []byte{
		0x17, 0x20, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x39, 0x00, 0x00, 0x00, 0x01, 0x00, 0x03, 0x01,
		0xaa, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x04, 0x19, 0x17, 0x00, 0x09, 0x06, 0x01, 0x00, 0x01, 0x01,
		0x00, 0x00, 0x01, 0x01, 0x09, 0x14, 0x37, 0x02, 0x11, 0x00, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00,
		0x2b, 0x04, 0x01, 0x19, 0x04, 0x20, 0x00, 0x00, 0x93, 0x26, 0x04, 0x88, 0x08, 0x92, 0x00, 0x00,
	}

	response, err := UnmarshalResponse(message)

	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if response == nil {
		t.Fatalf("Unexpected response: %v\n", response)
	}

	reply, ok := response.(*GetStatusResponse)
	if !ok {
		t.Fatalf("Invalid response type - expected:%T, got: %T\n", &GetStatusResponse{}, response)
	}

	if reply.MsgType != 0x20 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x32, reply.MsgType)
	}

	if reply.SerialNumber != 423187757 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 423187757, reply.SerialNumber)
	}

	if reply.EventIndex != 57 {
		t.Errorf("Incorrect 'last index' - expected:%v, got:%v", 57, reply.EventIndex)
	}

	if reply.EventType != 1 {
		t.Errorf("Incorrect 'event type' - expected:%v, got:%v", 1, reply.EventType)
	}

	if reply.Granted {
		t.Errorf("Incorrect 'access granted' - expected:%v, got:%v", false, reply.Granted)
	}

	if reply.Door != 3 {
		t.Errorf("Incorrect 'door' - expected:%v, got:%v", 3, reply.Door)
	}

	if reply.Direction != 0x01 {
		t.Errorf("Incorrect 'direction' - expected:%v, got:%v", 0x01, reply.Direction)
	}

	if reply.CardNumber != 6154410 {
		t.Errorf("Incorrect 'card number' - expected:%v, got:%v", 6154410, reply.CardNumber)
	}

	swiped, _ := time.ParseInLocation("2006-01-02 15:04:05", "2019-04-19 17:00:09", time.Local)
	if reply.Timestamp.IsZero() || reply.Timestamp != types.DateTime(swiped) {
		t.Errorf("Incorrect 'event timestamp' - expected:%v, got:%v", swiped.Format("2006-01-02 15:04:05"), reply.Timestamp)
	}

	if reply.Reason != 6 {
		t.Errorf("Incorrect 'event reason' - expected:%v, got:%v", 6, reply.Reason)
	}

	if !reply.Door1State {
		t.Errorf("Incorrect 'door 1 state' - expected:%v, got:%v", true, reply.Door1State)
	}

	if reply.Door2State {
		t.Errorf("Incorrect 'door 2 state' - expected:%v, got:%v", false, reply.Door2State)
	}

	if !reply.Door3State {
		t.Errorf("Incorrect 'door 3 state' - expected:%v, got:%v", true, reply.Door3State)
	}

	if !reply.Door4State {
		t.Errorf("Incorrect 'door 4 state' - expected:%v, got:%v", true, reply.Door4State)
	}

	if reply.Door1Button {
		t.Errorf("Incorrect 'door 1 button' - expected:%v, got:%v", false, reply.Door1Button)
	}

	if reply.Door2Button {
		t.Errorf("Incorrect 'door 2 button' - expected:%v, got:%v", false, reply.Door2Button)
	}

	if !reply.Door3Button {
		t.Errorf("Incorrect 'door 3 button' - expected:%v, got:%v", true, reply.Door3Button)
	}

	if !reply.Door4Button {
		t.Errorf("Incorrect 'door 4 button' - expected:%v, got:%v", true, reply.Door4Button)
	}

	if reply.SystemError != 9 {
		t.Errorf("Incorrect 'system error' - expected:%v, got:%v", 9, reply.SystemError)
	}

	sysdate, _ := time.ParseInLocation("2006-01-02", "2019-04-20", time.Local)
	if reply.SystemDate != types.SystemDate(sysdate) {
		t.Errorf("Incorrect 'system date' - expected:%v, got:%v", sysdate.Format("2006-01-02"), reply.SystemDate)
	}

	systime, _ := time.ParseInLocation("15:04:05", "14:37:02", time.Local)
	if reply.SystemTime != types.SystemTime(systime) {
		t.Errorf("Incorrect 'system time' - expected:%v, got:%v", systime.Format("15:04:05"), reply.SystemTime)
	}

	if reply.SequenceId != 17 {
		t.Errorf("Incorrect 'sequence ID' - expected:%v, got:%v", 17, reply.SequenceId)
	}

	if reply.SpecialInfo != 43 {
		t.Errorf("Incorrect 'special info' - expected:%v, got:%v", 43, reply.SpecialInfo)
	}

	if reply.RelayState != 0x04 {
		t.Errorf("Incorrect 'relay state' - expected:%v, got:%v", 0x04, reply.RelayState)
	}

	if reply.InputState != 0x01 {
		t.Errorf("Incorrect 'input state' - expected:%v, got:%v", 0x01, reply.InputState)
	}
}

func TestUnmarshalGetStatusResponseWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x92,
		0x20, 0x18, 0x08, 0x16, 0x20, 0x18, 0x12, 0x31, 0x12, 0x23, 0x34, 0x01, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := GetStatusResponse{}

	err := codec.Unmarshal(message, &reply)
	if err == nil {
		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0x92, received 0x94")
	}
}

// Ref. https://github.com/uhppoted/uhppote-cli/issues/4
// Ref. https://github.com/uhppoted/uhppoted-rest/issues/3
func TestUnmarshalGetStatusResponseWithNoEvent(t *testing.T) {
	message := []byte{
		0x17, 0x20, 0x00, 0x00, 0xe5, 0xd8, 0x4f, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x16, 0x03, 0x34, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x21, 0x04, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	response, err := UnmarshalResponse(message)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	} else if response == nil {
		t.Fatalf("Unexpected response: %v\n", response)
	}

	reply, ok := response.(*GetStatusResponse)
	if !ok {
		t.Fatalf("Invalid response type - expected:%T, got: %T\n", &GetStatusResponse{}, response)
	}

	if reply.MsgType != 0x20 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x32, reply.MsgType)
	}

	if reply.SerialNumber != 223336677 {
		t.Errorf("Incorrect 'serial number' - expected:%v, got:%v", 223336677, reply.SerialNumber)
	}

	if reply.EventIndex != 0 {
		t.Errorf("Incorrect 'last index' - expected:%v, got:%v", 0, reply.EventIndex)
	}

	if reply.EventType != 0 {
		t.Errorf("Incorrect 'event type' - expected:%v, got:%v", 0, reply.EventType)
	}

	if reply.Granted {
		t.Errorf("Incorrect 'access granted' - expected:%v, got:%v", false, reply.Granted)
	}

	if reply.Door != 0 {
		t.Errorf("Incorrect 'door' - expected:%v, got:%v", 0, reply.Door)
	}

	if reply.Direction != 0x00 {
		t.Errorf("Incorrect 'direction' - expected:%v, got:%v", 0x00, reply.Direction)
	}

	if reply.CardNumber != 0 {
		t.Errorf("Incorrect 'card number' - expected:%v, got:%v", 0, reply.CardNumber)
	}

	if !reply.Timestamp.IsZero() {
		t.Errorf("Incorrect 'event timestamp' - expected:%v, got:%v", types.DateTime{}, reply.Timestamp)
	}

	if reply.Reason != 0 {
		t.Errorf("Incorrect 'event reason' - expected:%v, got:%v", 0, reply.Reason)
	}

	if !reply.Door1State {
		t.Errorf("Incorrect 'door 1 state' - expected:%v, got:%v", true, reply.Door1State)
	}

	if !reply.Door2State {
		t.Errorf("Incorrect 'door 2 state' - expected:%v, got:%v", true, reply.Door2State)
	}

	if reply.Door3State {
		t.Errorf("Incorrect 'door 3 state' - expected:%v, got:%v", false, reply.Door3State)
	}

	if reply.Door4State {
		t.Errorf("Incorrect 'door 4 state' - expected:%v, got:%v", false, reply.Door4State)
	}

	if reply.Door1Button {
		t.Errorf("Incorrect 'door 1 button' - expected:%v, got:%v", false, reply.Door1Button)
	}

	if reply.Door2Button {
		t.Errorf("Incorrect 'door 2 button' - expected:%v, got:%v", false, reply.Door2Button)
	}

	if reply.Door3Button {
		t.Errorf("Incorrect 'door 3 button' - expected:%v, got:%v", false, reply.Door3Button)
	}

	if reply.Door4Button {
		t.Errorf("Incorrect 'door 4 button' - expected:%v, got:%v", false, reply.Door4Button)
	}

	if reply.SystemError != 0 {
		t.Errorf("Incorrect 'system error' - expected:%v, got:%v", 0, reply.SystemError)
	}

	sysdate, _ := time.ParseInLocation("2006-01-02", "2021-04-08", time.Local)
	if reply.SystemDate != types.SystemDate(sysdate) {
		t.Errorf("Incorrect 'system date' - expected:%v, got:%v", sysdate.Format("2006-01-02"), reply.SystemDate)
	}

	systime, _ := time.ParseInLocation("15:04:05", "16:03:34", time.Local)
	if reply.SystemTime != types.SystemTime(systime) {
		t.Errorf("Incorrect 'system time' - expected:%v, got:%v", systime.Format("15:04:05"), reply.SystemTime)
	}

	if reply.SequenceId != 0 {
		t.Errorf("Incorrect 'sequence ID' - expected:%v, got:%v", 0, reply.SequenceId)
	}

	if reply.SpecialInfo != 0 {
		t.Errorf("Incorrect 'special info' - expected:%v, got:%v", 0, reply.SpecialInfo)
	}

	if reply.RelayState != 0x00 {
		t.Errorf("Incorrect 'relay state' - expected:%v, got:%v", 0x00, reply.RelayState)
	}

	if reply.InputState != 0x00 {
		t.Errorf("Incorrect 'input state' - expected:%v, got:%v", 0x00, reply.InputState)
	}
}

func mustParseSystemDate(s string) types.SystemDate {
	if yymmdd, err := time.ParseInLocation("060102", s, time.Local); err != nil {
		return types.SystemDate(time.Time{})
	} else {
		return types.SystemDate(yymmdd)
	}
}

func mustParseSystemTime(s string) types.SystemTime {
	if hhmmss, err := time.ParseInLocation("15:04:05", s, time.Local); err != nil {
		return types.SystemTime(time.Time{})
	} else {
		return types.SystemTime(hhmmss)
	}
}
