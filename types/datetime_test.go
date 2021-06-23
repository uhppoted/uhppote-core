package types

import (
	"fmt"
	"testing"
	"time"
)

func TestDateTimeStringer(t *testing.T) {
	expected := "2021-04-09 12:18:37"
	datetime := DateTime(time.Date(2021, time.April, 9, 12, 18, 37, 0, time.Local))

	if s := fmt.Sprintf("%v", datetime); s != expected {
		t.Errorf("Invalid date/time string - expected:%v, got:%v", expected, s)
	}

	if s := fmt.Sprintf("%v", &datetime); s != expected {
		t.Errorf("Invalid date/time string - expected:%v, got:%v", expected, s)
	}
}

func TestDateTimeToString(t *testing.T) {
	expected := "2021-04-09 12:18:37"
	datetime := DateTime(time.Date(2021, time.April, 9, 12, 18, 37, 0, time.Local))

	if s := DateTimeToString(&datetime); s != expected {
		t.Errorf("Invalid date/time string - expected:%v, got:%v", expected, s)
	}

	if s := DateTimeToString(nil); s != "" {
		t.Errorf("Invalid date/time string - expected:'%v', got:%v", "", s)
	}
}
