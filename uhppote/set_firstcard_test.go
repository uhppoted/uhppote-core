package uhppote

import (
	"fmt"
	"net"
	"slices"
	"testing"
	"time"

	"github.com/uhppoted/uhppote-core/types"
)

func TestSetFirstCard(t *testing.T) {
	rq := []byte{
		0x17, 0xaa, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x08, 0x30, 0x01, 0x16, 0x45, 0x03, 0x01,
		0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	message := []byte{
		0x17, 0xaa, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	firstcard := types.FirstCard{
		StartTime: types.MustParseHHmm("08:30"),
		EndTime:   types.MustParseHHmm("16:45"),
		Active:    types.ModeNormallyOpen,
		Inactive:  types.ModeFirstCardOnly,
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	u := uhppote{
		driver: &stub{
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				if slices.Equal(request, rq) {
					return message, nil
				}

				t.Errorf("Unrecognised set-firstcard request")

				return nil, nil
			},
		},
	}

	ok, err := u.SetFirstCard(405419896, 3, firstcard)
	if err != nil {
		t.Fatalf("Unexpected error returned from SetFirstCard (%v)", err)
	}

	if !ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", true, ok)
	}
}

func TestSetFirstCardWithInvalidResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5c, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	firstcard := types.FirstCard{
		StartTime: types.MustParseHHmm("08:30"),
		EndTime:   types.MustParseHHmm("16:45"),
		Active:    types.NormallyOpen,
		Inactive:  types.NormallyClosed,
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	u := uhppote{
		driver: &stub{
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	_, err := u.SetFirstCard(405419896, 3, firstcard)
	if err == nil {
		t.Fatalf("Expected error from SetFirstCard (%v)", err)
	}
}

func TestSetFirstCardWithInvalidDeviceID(t *testing.T) {
	firstcard := types.FirstCard{
		StartTime: types.MustParseHHmm("08:30"),
		EndTime:   types.MustParseHHmm("16:45"),
		Active:    types.NormallyOpen,
		Inactive:  types.NormallyClosed,
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	u := uhppote{}

	_, err := u.SetFirstCard(0, 3, firstcard)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}

func TestSetFirstCardWithInvalidActiveMode(t *testing.T) {
	firstcard := types.FirstCard{
		StartTime: types.MustParseHHmm("08:30"),
		EndTime:   types.MustParseHHmm("16:45"),
		Active:    types.ModeUnknown,
		Inactive:  types.NormallyClosed,
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	u := uhppote{}

	_, err := u.SetFirstCard(405419896, 3, firstcard)
	if err == nil {
		t.Fatalf("Expected 'Invalid first-card active mode' error, got %v", err)
	} else if fmt.Sprintf("%v", err) != "invalid first-card active mode (unknown)" {
		t.Errorf("Expected 'Invalid first-card active mode' error, got %v", err)
	}
}

func TestSetFirstCardWithFirstCardOnlyActiveMode(t *testing.T) {
	firstcard := types.FirstCard{
		StartTime: types.MustParseHHmm("08:30"),
		EndTime:   types.MustParseHHmm("16:45"),
		Active:    types.FirstCardOnly,
		Inactive:  types.FirstCardOnly,
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	u := uhppote{}

	_, err := u.SetFirstCard(405419896, 3, firstcard)
	if err == nil {
		t.Fatalf("Expected 'Invalid first-card active mode' error, got %v", err)
	} else if fmt.Sprintf("%v", err) != "invalid first-card active mode (firstcard only)" {
		t.Errorf("Expected 'Invalid first-card active mode' error, got %v", err)
	}
}

func TestSetFirstCardWithInvalidInactiveMode(t *testing.T) {
	firstcard := types.FirstCard{
		StartTime: types.MustParseHHmm("08:30"),
		EndTime:   types.MustParseHHmm("16:45"),
		Active:    types.NormallyOpen,
		Inactive:  types.ModeUnknown,
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	u := uhppote{}

	_, err := u.SetFirstCard(405419896, 3, firstcard)
	if err == nil {
		t.Fatalf("Expected 'Invalid first-card inactive mode' error, got %v", err)
	} else if fmt.Sprintf("%v", err) != "invalid first-card inactive mode (unknown)" {
		t.Errorf("Expected 'Invalid first-card inactive mode' error, got %v", err)
	}
}
