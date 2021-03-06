package types

import (
	"encoding/json"
	"fmt"
	"reflect"
	"testing"
	"time"
)

var date = func(s string) Date {
	d, _ := time.ParseInLocation("2006-01-02", s, time.Local)
	return Date(d)
}

func TestCardToString(t *testing.T) {
	expected := "12345    2020-01-01 2020-12-31 Y N 29 Y"

	from := date("2020-01-01")
	to := date("2020-12-31")
	card := Card{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors: map[uint8]int{
			1: 1,
			2: 0,
			3: 29,
			4: 1,
		},
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestCardToStringWithMissingFromDate(t *testing.T) {
	expected := "12345    -          2020-12-31 Y N N Y"

	to := date("2020-12-31")
	card := Card{
		CardNumber: 12345,
		To:         &to,
		Doors: map[uint8]int{
			1: 1,
			2: 0,
			3: 0,
			4: 1,
		},
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestCardToStringWithMissingToDate(t *testing.T) {
	expected := "12345    2020-01-01 -          Y N N Y"

	from := date("2020-01-01")
	card := Card{
		CardNumber: 12345,
		From:       &from,
		Doors: map[uint8]int{
			1: 1,
			2: 0,
			3: 0,
			4: 1,
		},
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestCardToStringWithMissingDoors(t *testing.T) {
	expected := "12345    2020-01-01 2020-12-31 Y N N N"

	from := date("2020-01-01")
	to := date("2020-12-31")
	card := Card{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors: map[uint8]int{
			1: 1,
			2: 0,
		},
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestCardToStringWithExtraDoors(t *testing.T) {
	expected := "12345    2020-01-01 2020-12-31 Y N N Y"

	from := date("2020-01-01")
	to := date("2020-12-31")
	card := Card{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors: map[uint8]int{
			1: 1,
			2: 0,
			3: 0,
			4: 1,
			5: 1,
			6: 0,
		},
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestMarshalCard(t *testing.T) {
	from := date("2020-01-01")
	to := date("2020-12-31")

	card := Card{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors: map[uint8]int{
			1: 1,
			2: 0,
			3: 29,
			4: 1,
		},
	}

	expected := `{
  "card-number": 12345,
  "start-date": "2020-01-01",
  "end-date": "2020-12-31",
  "doors": {
    "1": 1,
    "2": 0,
    "3": 29,
    "4": 1
  }
}`

	blob, err := json.MarshalIndent(card, "", "  ")
	if err != nil {
		t.Errorf("Unexpected error unmarshalling card (%v)", err)
	}

	if !reflect.DeepEqual(string(blob), expected) {
		t.Errorf("Card incorrectly unmarshalled\n   expected:%v\n   got:     %v", expected, string(blob))

	}
}

func TestUnmarshalCard(t *testing.T) {
	from := date("2020-01-01")
	to := date("2020-12-31")

	expected := Card{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors: map[uint8]int{
			1: 1,
			2: 0,
			3: 29,
			4: 1,
		},
	}

	blob := `{
  "card-number": 12345,
  "start-date": "2020-01-01",
  "end-date": "2020-12-31",
  "doors": {
    "1": 1,
    "2": 0,
    "3": 29,
    "4": 1
  }
} `

	card := Card{}
	err := json.Unmarshal([]byte(blob), &card)

	if err != nil {
		t.Errorf("Unexpected error unmarshalling card (%v)", err)
	}

	if !reflect.DeepEqual(card, expected) {
		t.Errorf("Card incorrectly unmarshalled\n   expected:%#v\n   got:     %#v", expected, card)
	}
}

func TestUnmarshalCardWithMissingCardNumber(t *testing.T) {
	from := date("2020-01-01")
	to := date("2020-12-31")

	expected := Card{
		CardNumber: 0,
		From:       &from,
		To:         &to,
		Doors: map[uint8]int{
			1: 1,
			2: 0,
			3: 0,
			4: 1,
		},
	}

	blob := `{
  "start-date": "2020-01-01",
  "end-date": "2020-12-31",
  "doors": {
    "1":1,
    "2":0,
    "3":0,
    "4":1
  }
} `

	card := Card{}
	err := json.Unmarshal([]byte(blob), &card)

	if err != nil {
		t.Errorf("Unexpected error unmarshalling card (%v)", err)
	}

	if !reflect.DeepEqual(card, expected) {
		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
	}
}

func TestUnmarshalCardWithMissingStartDate(t *testing.T) {
	blob := `{
  "card-number": 12345,
  "end-date": "2020-12-31",
  "doors": [
    "1": true,
    "2": false,
    "3": false,
    "4": true
  ]
} `
	card := Card{}
	err := json.Unmarshal([]byte(blob), &card)

	if err == nil {
		t.Errorf("Expected error unmarshalling card without start-date")
	}
}

func TestUnmarshalCardWithMissingEndDate(t *testing.T) {
	blob := `{
  "card-number": 12345,
  "start-date": "2020-01-01",
  "doors": [
    "1": true,
    "2": false,
    "3": false,
    "4": true
  ]
} `
	card := Card{}
	err := json.Unmarshal([]byte(blob), &card)

	if err == nil {
		t.Errorf("Expected error unmarshalling card without end-date")
	}
}

func TestUnmarshalCardWithMissingDoors(t *testing.T) {
	from := date("2020-01-01")
	to := date("2020-12-31")

	expected := Card{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors: map[uint8]int{
			1: 0,
			2: 1,
			3: 0,
			4: 0,
		},
	}

	blob := `{
  "card-number": 12345,
  "start-date": "2020-01-01",
  "end-date": "2020-12-31",
  "doors": {
    "2": 1
  }
} `

	card := Card{}
	err := json.Unmarshal([]byte(blob), &card)

	if err != nil {
		t.Fatalf("Unexpected error unmarshalling card (%v)", err)
	}

	if !reflect.DeepEqual(card, expected) {
		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
	}
}

func TestUnmarshalCardWithInvalidDoors(t *testing.T) {
	from := date("2020-01-01")
	to := date("2020-12-31")

	expected := Card{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors: map[uint8]int{
			1: 1,
			2: 0,
			3: 0,
			4: 0,
		},
	}

	blob := `{
  "card-number": 12345,
  "start-date": "2020-01-01",
  "end-date": "2020-12-31",
  "doors": {
    "1": 1,
    "5": 0
  }
} `
	card := Card{}
	err := json.Unmarshal([]byte(blob), &card)

	if err != nil {
		t.Fatalf("Unexpected error unmarshalling card with invalid doors (%v)", err)
	}

	if !reflect.DeepEqual(card, expected) {
		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
	}
}
