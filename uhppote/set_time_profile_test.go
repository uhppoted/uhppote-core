package uhppote

import (
	"net"
	"testing"
	"time"

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
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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
		ID:              4,
		LinkedProfileID: 19,
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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

func TestSetTimeProfileWithMissingSegment3(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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

func TestSetTimeProfileWithMissingSegment1End(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("09:45")},
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("07:00")},
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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

func TestSetTimeProfileWithMissingSegment2End(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	profile := types.TimeProfile{
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: types.Segment{Start: hhmm("13:15")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: types.Segment{Start: hhmm("11:35")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: types.Segment{Start: hhmm("18:00"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: types.Segment{Start: hhmm("14:01")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
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
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
			},
		},
	}

	_, err := u.SetTimeProfile(423187757, profile)
	if err == nil {
		t.Fatalf("Expected error from SetTimeProfile (%v)", err)
	}
}

func TestSetTimeProfileWithInvalidDeviceID(t *testing.T) {
	profile := types.TimeProfile{
		ID:              4,
		LinkedProfileID: 19,
		From:            date("2021-04-01"),
		To:              date("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: types.Segments{
			1: types.Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: types.Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: types.Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	u := uhppote{}

	_, err := u.SetTimeProfile(0, profile)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
