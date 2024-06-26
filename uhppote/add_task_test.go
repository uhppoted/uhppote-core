package uhppote

import (
	"net"
	"testing"
	"time"

	"github.com/uhppoted/uhppote-core/types"
)

func TestAddTask(t *testing.T) {
	message := []byte{
		0x17, 0xa8, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	task := types.Task{
		Task: types.EnableTimeProfile,
		Door: 3,
		From: types.MustParseDate("2021-04-01"),
		To:   types.MustParseDate("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Start: hhmm("08:30"),
		Cards: 7,
	}

	u := uhppote{
		driver: &stub{
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	ok, err := u.AddTask(423187757, task)
	if err != nil {
		t.Fatalf("Unexpected error returned from AddTask (%v)", err)
	}

	if !ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", true, ok)
	}
}

func TestAddTaskWithInvalidFromDate(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	task := types.Task{
		Task: types.EnableTimeProfile,
		Door: 3,
		To:   types.MustParseDate("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Start: hhmm("08:30"),
		Cards: 7,
	}

	u := uhppote{
		driver: &stub{
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	ok, err := u.AddTask(423187757, task)
	if err == nil {
		t.Fatalf("Expected error returned from AddTask (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestAddTaskWithInvalidToDate(t *testing.T) {
	message := []byte{
		0x17, 0x88, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	task := types.Task{
		Task: types.EnableTimeProfile,
		Door: 3,
		From: types.MustParseDate("2021-04-01"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Start: hhmm("08:30"),
		Cards: 7,
	}

	u := uhppote{
		driver: &stub{
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	ok, err := u.AddTask(423187757, task)
	if err == nil {
		t.Fatalf("Expected error returned from AddTask (%v)", err)
	}

	if ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", false, ok)
	}
}

func TestAddTaskWithInvalidResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5c, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	task := types.Task{
		Task: types.EnableTimeProfile,
		Door: 3,
		From: types.MustParseDate("2021-04-01"),
		To:   types.MustParseDate("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Start: hhmm("08:30"),
		Cards: 7,
	}

	u := uhppote{
		driver: &stub{
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	_, err := u.AddTask(423187757, task)
	if err == nil {
		t.Fatalf("Expected error from AddTask (%v)", err)
	}
}

func TestAddTaskWithInvalidDeviceID(t *testing.T) {
	task := types.Task{
		Task: types.EnableTimeProfile,
		Door: 3,
		From: types.MustParseDate("2021-04-01"),
		To:   types.MustParseDate("2021-12-29"),
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Start: hhmm("08:30"),
		Cards: 7,
	}

	u := uhppote{}

	_, err := u.AddTask(0, task)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
