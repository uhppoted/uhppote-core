package types

import (
	"testing"
	"time"
)

func TestDateTimeStringer(t *testing.T) {
	expected := "2021-04-09 12:18:37"
	datetime := DateTime(time.Date(2021, time.April, 9, 12, 18, 37, 0, time.Local))
	datetimePtr := &datetime
	var nilDateTime *DateTime

	if datetime.String() != expected {
		t.Errorf("Invalid date/time string - expected:%v, got:%v", expected, datetime)
	}

	if datetimePtr.String() != expected {
		t.Errorf("Invalid date/time string - expected:%v, got:%v", expected, datetimePtr)
	}

	if nilDateTime.String() != "" {
		t.Errorf("Invalid date/time string - expected:'%v', got:%v", "", nilDateTime)
	}
}
