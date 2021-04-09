package messages

import (
	"reflect"
	"testing"
	"time"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/types"
)

func TestMarshalEvent(t *testing.T) {
	expected := []byte{
		0x17, 0x20, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x39, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
		0xaa, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x04, 0x19, 0x17, 0x00, 0x09, 0x06, 0x01, 0x00, 0x01, 0x01,
		0x00, 0x00, 0x01, 0x01, 0x09, 0x14, 0x37, 0x02, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x2b, 0x04, 0x01, 0x19, 0x04, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	swiped, _ := time.ParseInLocation("2006-01-02 15:04:05", "2019-04-19 17:00:09", time.Local)
	sysdate, _ := time.ParseInLocation("2006-01-02", "2019-04-20", time.Local)
	systime, _ := time.ParseInLocation("15:04:05", "14:37:02", time.Local)

	event := Event{
		SerialNumber: 423187757,
		EventIndex:   57,
		EventType:    2,
		Granted:      true,
		Door:         3,
		Direction:    0x01,
		CardNumber:   6154410,
		Timestamp:    types.DateTime(swiped),
		Reason:       6,
		Door1State:   true,
		Door2State:   false,
		Door3State:   true,
		Door4State:   true,
		Door1Button:  false,
		Door2Button:  false,
		Door3Button:  true,
		Door4Button:  true,
		SystemError:  0x09,
		SystemDate:   types.SystemDate(sysdate),
		SystemTime:   types.SystemTime(systime),
		SequenceId:   17,
		SpecialInfo:  43,
		RelayState:   0x04,
		InputState:   0x01,
	}

	m, err := codec.Marshal(event)
	if err != nil {
		t.Errorf("Unexpected error: %v\n", err)
	}

	if !reflect.DeepEqual(m, expected) {
		t.Fatalf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", dump(expected, ""), dump(m, ""))
	}
}

func TestUnmarshalEvent(t *testing.T) {
	message := []byte{
		0x17, 0x20, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x39, 0x00, 0x00, 0x00, 0x01, 0x00, 0x03, 0x01,
		0xaa, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x04, 0x19, 0x17, 0x00, 0x09, 0x06, 0x01, 0x00, 0x01, 0x01,
		0x00, 0x00, 0x01, 0x01, 0x09, 0x14, 0x37, 0x02, 0x11, 0x00, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00,
		0x2b, 0x04, 0x01, 0x19, 0x04, 0x20, 0x00, 0x00, 0x93, 0x26, 0x04, 0x88, 0x08, 0x92, 0x00, 0x00,
	}

	reply := Event{}

	err := codec.Unmarshal(message, &reply)

	if err != nil {
		t.Errorf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x20 {
		t.Errorf("Incorrect 'message type' - expected:%02X, got:%02x", 0x20, reply.MsgType)
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
	if reply.Timestamp != types.DateTime(swiped) {
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
		t.Errorf("Incorrect 'system date' - expected:%s, got:%s", sysdate.Format("2006-01-02"), reply.SystemDate.String())
	}

	systime, _ := time.ParseInLocation("15:04:05", "14:37:02", time.Local)
	if reply.SystemTime != types.SystemTime(systime) {
		t.Errorf("Incorrect 'system time' - expected:%s, got:%s", systime.Format("15:04:05"), reply.SystemTime.String())
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

func TestUnmarshalEventWithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x92,
		0x20, 0x18, 0x08, 0x16, 0x20, 0x18, 0x12, 0x31, 0x12, 0x23, 0x34, 0x01, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := Event{}

	err := codec.Unmarshal(message, &reply)

	if err == nil {
		t.Errorf("Expected error: '%v'", "Invalid value in message - expected 0x20, received 0x94")
		return
	}
}

func TestMarshalEventV6_62(t *testing.T) {
	expected := []byte{
		0x19, 0x20, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x39, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
		0xaa, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x04, 0x19, 0x17, 0x00, 0x09, 0x06, 0x01, 0x00, 0x01, 0x01,
		0x00, 0x00, 0x01, 0x01, 0x09, 0x14, 0x37, 0x02, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x2b, 0x04, 0x01, 0x19, 0x04, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	swiped, _ := time.ParseInLocation("2006-01-02 15:04:05", "2019-04-19 17:00:09", time.Local)
	sysdate, _ := time.ParseInLocation("2006-01-02", "2019-04-20", time.Local)
	systime, _ := time.ParseInLocation("15:04:05", "14:37:02", time.Local)

	event := EventV6_62{
		Event: Event{
			SerialNumber: 423187757,
			EventIndex:   57,
			EventType:    2,
			Granted:      true,
			Door:         3,
			Direction:    0x01,
			CardNumber:   6154410,
			Timestamp:    types.DateTime(swiped),
			Reason:       6,
			Door1State:   true,
			Door2State:   false,
			Door3State:   true,
			Door4State:   true,
			Door1Button:  false,
			Door2Button:  false,
			Door3Button:  true,
			Door4Button:  true,
			SystemError:  0x09,
			SystemDate:   types.SystemDate(sysdate),
			SystemTime:   types.SystemTime(systime),
			SequenceId:   17,
			SpecialInfo:  43,
			RelayState:   0x04,
			InputState:   0x01,
		},
	}

	m, err := codec.Marshal(event)
	if err != nil {
		t.Errorf("Unexpected error: %v\n", err)
	}

	if !reflect.DeepEqual(m, expected) {
		t.Fatalf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", dump(expected, ""), dump(m, ""))
	}
}

func TestUnmarshalEventV6_62(t *testing.T) {
	message := []byte{
		0x19, 0x20, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x39, 0x00, 0x00, 0x00, 0x01, 0x00, 0x03, 0x01,
		0xaa, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x04, 0x19, 0x17, 0x00, 0x09, 0x06, 0x01, 0x00, 0x01, 0x01,
		0x00, 0x00, 0x01, 0x01, 0x09, 0x14, 0x37, 0x02, 0x11, 0x00, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00,
		0x2b, 0x04, 0x01, 0x19, 0x04, 0x20, 0x00, 0x00, 0x93, 0x26, 0x04, 0x88, 0x08, 0x92, 0x00, 0x00,
	}

	reply := EventV6_62{}

	err := codec.Unmarshal(message, &reply)
	if err != nil {
		t.Errorf("Unexpected error: %v\n", err)
	}

	if reply.MsgType != 0x20 {
		t.Errorf("Incorrect 'message type' - expected:0x%02X, got:0x%02x", 0x20, reply.MsgType)
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
	if reply.Timestamp != types.DateTime(swiped) {
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
		t.Errorf("Incorrect 'system date' - expected:%s, got:%s", sysdate.Format("2006-01-02"), reply.SystemDate.String())
	}

	systime, _ := time.ParseInLocation("15:04:05", "14:37:02", time.Local)
	if reply.SystemTime != types.SystemTime(systime) {
		t.Errorf("Incorrect 'system time' - expected:%s, got:%s", systime.Format("15:04:05"), reply.SystemTime.String())
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

func TestUnmarshalEventV6_62WithInvalidMsgType(t *testing.T) {
	message := []byte{
		0x19, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x92,
		0x20, 0x18, 0x08, 0x16, 0x20, 0x18, 0x12, 0x31, 0x12, 0x23, 0x34, 0x01, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	reply := EventV6_62{}

	err := codec.Unmarshal(message, &reply)

	if err == nil {
		t.Errorf("Expected error: '%v'", "Invalid value in message - expected 0x20, received 0x94")
		return
	}
}
