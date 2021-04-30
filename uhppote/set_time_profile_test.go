package uhppote

import (
	"testing"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/types"
)

func TestSetTimeProfile(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err != nil {
		t.Fatalf("Unexpected error returned from SetTimeProfile (%v)", err)
	}

	if !ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", true, ok)
	}
}

func TestSetTimeProfileWithInvalidFromDate(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
		ProfileID:       4,
		LinkedProfileID: 19,
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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithInvalidToDate(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
		ProfileID:       4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),

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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithMissingSegment1(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithMissingSegment2(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Eexpected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithMissingSegment3(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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
		},
	}

	u := UHPPOTE{
		driver: &mock{
			send: func(deviceID uint32, request, response interface{}) error {
				return codec.Unmarshal(message, response)
			},
		},
	}

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithInvalidSegment1Start(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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
				End: hhmm("09:45"),
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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithInvalidSegment1End(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithInvalidSegment2Start(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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
				End: hhmm("13:15"),
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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithInvalidSegment2End(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithInvalidSegment3Start(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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
				End: hhmm("17:59"),
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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithInvalidSegment3End(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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

	ok, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error returned from SetTimeProfile (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestSetTimeProfileWithInvalidResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5c, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
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

	_, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error from SetTimeProfile (%v)", err)
	}
}
