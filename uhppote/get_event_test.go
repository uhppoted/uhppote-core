package uhppote

import (
	"fmt"
	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/types"
	"reflect"
	"testing"
	"time"
)

func TestGetEvent(t *testing.T) {
	message := []byte{
		0x17, 0xb0, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x08, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
		0xad, 0xe8, 0x5d, 0x00, 0x20, 0x19, 0x02, 0x10, 0x07, 0x12, 0x01, 0x06, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4a, 0x26, 0x80, 0x39, 0x08, 0x92, 0x00, 0x00,
	}

	u := uhppote{
		impl: &mock{
			send: func(deviceID uint32, request, response interface{}) error {
				return codec.Unmarshal(message, response)
			},
		},
	}

	timestamp, _ := time.ParseInLocation("2006-01-02 15:04:05", "2019-02-10 07:12:01", time.Local)
	datetime := types.DateTime(timestamp)
	expected := types.Event{
		SerialNumber: 423187757,
		Index:        8,
		Type:         2,
		Granted:      true,
		Door:         3,
		Direction:    1,
		CardNumber:   6154413,
		Timestamp:    datetime,
		Reason:       0x06,
	}

	response, err := u.GetEvent(423187757, 37)
	if err != nil {
		t.Fatalf("Unexpected error returned from GetEvent (%v)", err)
	}

	if response == nil {
		t.Fatalf("Expected response from GetEvent, got:%v", response)
	}

	if !reflect.DeepEqual(*response, expected) {
		t.Errorf("Invalid response:\nexpected:%#v\ngot:     %#v", expected, *response)
	}
}

func TestGetEventWithNoEvents(t *testing.T) {
	message := []byte{
		0x17, 0xb0, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	u := uhppote{
		impl: &mock{
			send: func(deviceID uint32, request, response interface{}) error {
				return codec.Unmarshal(message, response)
			},
		},
	}

	response, err := u.GetEvent(423187757, 37)
	if err != nil {
		t.Fatalf("Unexpected error returned from GetEvent (%v)", err)
	}

	if response != nil {
		t.Fatalf("Expected <nil> response from GetEvent, got:%#v", response)
	}
}

func TestGetEventWithError(t *testing.T) {
	u := uhppote{
		impl: &mock{
			send: func(deviceID uint32, request, reply interface{}) error {
				return fmt.Errorf("EXPECTED")
			},
		},
	}

	_, err := u.GetEvent(423187757, 37)
	if err == nil {
		t.Fatalf("Expected error return from GetEvent")
	}
}
