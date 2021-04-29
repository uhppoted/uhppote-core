package uhppote

import (
	"reflect"
	"testing"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/types"
)

func TestGetTimeProfile(t *testing.T) {
	message := []byte{
		0x17, 0x98, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x04, 0x20, 0x21, 0x04, 0x01, 0x20, 0x21, 0x12,
		0x29, 0x01, 0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x08, 0x30, 0x09, 0x45, 0x11, 0x35, 0x13, 0x15,
		0x14, 0x01, 0x17, 0x59, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := types.TimeProfile{
		ProfileID:       4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),

		Weekdays: struct {
			Monday    bool `json:"monday"`
			Tuesday   bool `json:"tuesday"`
			Wednesday bool `json:"wednesday"`
			Thursday  bool `json:"thursday"`
			Friday    bool `json:"friday"`
			Saturday  bool `json:"saturday"`
			Sunday    bool `json:"sunday"`
		}{
			Monday:    true,
			Tuesday:   true,
			Wednesday: false,
			Thursday:  true,
			Friday:    false,
			Saturday:  true,
			Sunday:    true,
		},

		Segments: map[uint8]struct {
			Start *types.HHmm `json:"start"`
			End   *types.HHmm `json:"end"`
		}{
			1: struct {
				Start *types.HHmm `json:"start"`
				End   *types.HHmm `json:"end"`
			}{
				Start: hhmm("08:30"),
				End:   hhmm("09:45"),
			},
			2: struct {
				Start *types.HHmm `json:"start"`
				End   *types.HHmm `json:"end"`
			}{
				Start: hhmm("11:35"),
				End:   hhmm("13:15"),
			},
			3: struct {
				Start *types.HHmm `json:"start"`
				End   *types.HHmm `json:"end"`
			}{
				Start: hhmm("14:01"),
				End:   hhmm("17:59"),
			},
		},
	}

	u := UHPPOTE{
		driver: &mock{
			send: func(deviceID uint32, request, response interface{}) error {
				return codec.Unmarshal(message, response)
			},
		},
	}

	profile, err := u.GetTimeProfile(423187757, 4)
	if err != nil {
		t.Fatalf("Unexpected error returned from GetTimeProfile (%v)", err)
	} else if profile == nil {
		t.Fatalf("Expected response from GetTimeProfile, got:%v", profile)
	}

	if !reflect.DeepEqual(*profile, expected) {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", expected, *profile)
	}
}

//func TestGetTimeProfileWithInactiveProfile(t *testing.T) {
//	message := []byte{
//		0x17, 0x5c, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//	}
//
//	u := mock{
//		send: func(deviceID uint32, request, response interface{}) error {
//			return codec.Unmarshal(message, response)
//		},
//	}
//
//	card, err := getCardByIndex(&u, 423187757, 6129876)
//	if err != nil {
//		t.Fatalf("Unexpected error returned from GetCardByIndex (%v)", err)
//	}
//
//	if card != nil {
//		t.Fatalf("Expected <nil> from GetCardByIndex, got:%v", *card)
//	}
//}

//func TestGetTimeProfileWithInvalidResponse(t *testing.T) {
//	message := []byte{
//		0x17, 0x5c, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//	}
//
//	u := mock{
//		send: func(deviceID uint32, request, response interface{}) error {
//			return codec.Unmarshal(message, response)
//		},
//	}
//
//	card, err := getCardByIndex(&u, 423187757, 6129876)
//	if err != nil {
//		t.Fatalf("Unexpected error returned from GetCardByIndex (%v)", err)
//	}
//
//	if card != nil {
//		t.Fatalf("Expected <nil> from GetCardByIndex, got:%v", *card)
//	}
//}

func hhmm(s string) *types.HHmm {
	t, _ := types.HHmmFromString(s)

	return t
}
