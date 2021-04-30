package messages

import (
	"reflect"
	"testing"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	//	"github.com/uhppoted/uhppote-core/types"
)

func TestMarshalSetTimeProfileRequest(t *testing.T) {
	expected := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x04, 0x20, 0x21, 0x04, 0x01, 0x20, 0x21, 0x12,
		0x29, 0x01, 0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x08, 0x30, 0x09, 0x45, 0x11, 0x35, 0x13, 0x15,
		0x14, 0x01, 0x17, 0x59, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	request := SetTimeProfileRequest{
		SerialNumber:    423187757,
		ProfileID:       4,
		From:            *yyyymmdd("2021-04-01"),
		To:              *yyyymmdd("2021-12-29"),
		Monday:          true,
		Tuesday:         true,
		Wednesday:       false,
		Thursday:        true,
		Friday:          false,
		Saturday:        true,
		Sunday:          true,
		Segment1Start:   *hhmm("08:30"),
		Segment1End:     *hhmm("09:45"),
		Segment2Start:   *hhmm("11:35"),
		Segment2End:     *hhmm("13:15"),
		Segment3Start:   *hhmm("14:01"),
		Segment3End:     *hhmm("17:59"),
		LinkedProfileID: 19,
	}

	if m, err := codec.Marshal(request); err != nil {
		t.Fatalf("Unexpected error: %v", err)
	} else if !reflect.DeepEqual(m, expected) {
		t.Errorf("Invalid byte array:\nExpected:\n%s\nReturned:\n%s", dump(expected, ""), dump(m, ""))
	}
}

func TestFactoryUnmarshalSetTimeProfileRequest(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x04, 0x20, 0x21, 0x04, 0x01, 0x20, 0x21, 0x12,
		0x29, 0x01, 0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x08, 0x30, 0x09, 0x45, 0x11, 0x35, 0x13, 0x15,
		0x14, 0x01, 0x17, 0x59, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := SetTimeProfileRequest{
		MsgType:         0x88,
		SerialNumber:    423187757,
		ProfileID:       4,
		From:            *yyyymmdd("2021-04-01"),
		To:              *yyyymmdd("2021-12-29"),
		Monday:          true,
		Tuesday:         true,
		Wednesday:       false,
		Thursday:        true,
		Friday:          false,
		Saturday:        true,
		Sunday:          true,
		Segment1Start:   *hhmm("08:30"),
		Segment1End:     *hhmm("09:45"),
		Segment2Start:   *hhmm("11:35"),
		Segment2End:     *hhmm("13:15"),
		Segment3Start:   *hhmm("14:01"),
		Segment3End:     *hhmm("17:59"),
		LinkedProfileID: 19,
	}

	request, err := UnmarshalRequest(message)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	} else if request == nil {
		t.Fatalf("Unexpected request: %v\n", request)
	}

	rq, ok := request.(*SetTimeProfileRequest)
	if !ok {
		t.Fatalf("Invalid request type - expected:%T, got: %T\n", &SetTimeProfileRequest{}, request)
	}

	if !reflect.DeepEqual(*rq, expected) {
		t.Errorf("Incorrect request\n   expected:%+v\n   got:     %+v", expected, *rq)
	}
}

func TestUnmarshalSetTimeProfileResponse(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := SetTimeProfileResponse{
		MsgType:      0x88,
		SerialNumber: 423187757,
		Succeeded:    true,
	}

	reply := SetTimeProfileResponse{}

	if err := codec.Unmarshal(message, &reply); err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	}

	if !reflect.DeepEqual(reply, expected) {
		t.Errorf("Incorrect reply\n   expected:%+v\n   got:     %+v", expected, reply)
	}
}

func TestFactoryUnmarshalSetTimeProfileResponse(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := SetTimeProfileResponse{
		MsgType:      0x88,
		SerialNumber: 423187757,
		Succeeded:    true,
	}

	response, err := UnmarshalResponse(message)
	if err != nil {
		t.Fatalf("Unexpected error: %v\n", err)
	} else if response == nil {
		t.Fatalf("Unexpected response: %v\n", response)
	}

	reply, ok := response.(*SetTimeProfileResponse)
	if !ok {
		t.Fatalf("Invalid response type - expected:%T, got: %T\n", &SetTimeProfileResponse{}, response)
	}

	if !reflect.DeepEqual(*reply, expected) {
		t.Errorf("Incorrect reply\n   expected:%+v\n   got:     %+v", expected, reply)
	}
}

//func TestUnmarshalSetTimeProfileDeactivatedResponse(t *testing.T) {
//	message := []byte{
//		0x17, 0x98, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//	}
//
//	start, _ := types.HHmmFromString("00:00")
//	end, _ := types.HHmmFromString("00:00")
//
//	expected := SetTimeProfileResponse{
//		MsgType:         0x98,
//		SerialNumber:    423187757,
//		ProfileID:       0,
//		From:            nil,
//		To:              nil,
//		Monday:          false,
//		Tuesday:         false,
//		Wednesday:       false,
//		Thursday:        false,
//		Friday:          false,
//		Saturday:        false,
//		Sunday:          false,
//		Segment1Start:   start,
//		Segment1End:     end,
//		Segment2Start:   start,
//		Segment2End:     end,
//		Segment3Start:   start,
//		Segment3End:     end,
//		LinkedProfileID: 0,
//	}
//
//	reply := SetTimeProfileResponse{}
//
//	if err := codec.Unmarshal(message, &reply); err != nil {
//		t.Fatalf("Unexpected error: %v\n", err)
//	}
//
//	if !reflect.DeepEqual(reply, expected) {
//		t.Errorf("Incorrect reply\n   expected:%+v\n   got:     %+v", expected, reply)
//	}
//}
//
//func TestUnmarshalSetTimeProfileResponseWithInvalidMsgType(t *testing.T) {
//	message := []byte{
//		0x17, 0x94, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xc0, 0xa8, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x19, 0x39, 0x55, 0x2d, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x92,
//		0x20, 0x18, 0x08, 0x16, 0x20, 0x18, 0x12, 0x31, 0x12, 0x23, 0x34, 0x01, 0x00, 0x00, 0x00, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//	}
//
//	reply := SetTimeProfileResponse{}
//
//	if err := codec.Unmarshal(message, &reply); err == nil {
//		t.Fatalf("Expected error: '%v'", "Invalid value in message - expected 0x98, received 0x94")
//	}
//}
//
//func yyyymmdd(s string) *types.Date {
//	d, _ := types.DateFromString(s)
//
//	return d
//}
//
//func hhmm(s string) *types.HHmm {
//	t, _ := types.HHmmFromString(s)
//
//	return t
//}
