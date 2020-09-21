package types

import (
	"encoding/json"
	"fmt"
	"reflect"
	"testing"
)

func TestCardXToString(t *testing.T) {
	expected := "12345    2020-01-01 2020-12-31 Y N N Y"

	from := date("2020-01-01")
	to := date("2020-12-31")
	card := CardX{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true},
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestCardXToStringWithMissingFromDate(t *testing.T) {
	expected := "12345    -          2020-12-31 Y N N Y"

	to := date("2020-12-31")
	card := CardX{
		CardNumber: 12345,
		To:         &to,
		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true},
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestCardXToStringWithMissingToDate(t *testing.T) {
	expected := "12345    2020-01-01 -          Y N N Y"

	from := date("2020-01-01")
	card := CardX{
		CardNumber: 12345,
		From:       &from,
		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true},
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestCardXToStringWithMissingDoors(t *testing.T) {
	expected := "12345    2020-01-01 2020-12-31 Y N N N"

	from := date("2020-01-01")
	to := date("2020-12-31")
	card := CardX{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors:      map[uint8]bool{1: true, 2: false},
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestCardXToStringWithExtraDoors(t *testing.T) {
	expected := "12345    2020-01-01 2020-12-31 Y N N Y"

	from := date("2020-01-01")
	to := date("2020-12-31")
	card := CardX{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true, 5: true, 6: false},
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestUnmarshalCardX(t *testing.T) {
	from := date("2020-01-01")
	to := date("2020-12-31")

	expected := CardX{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true},
	}

	blob := `{
  "card-number": 12345,
  "start-date": "2020-01-01",
  "end-date": "2020-12-31",
  "doors": {
    "1": true,
    "2": false,
    "3": false,
    "4": true
  }
} `

	card := CardX{}
	err := json.Unmarshal([]byte(blob), &card)

	if err != nil {
		t.Errorf("Unexpected error unmarshalling card (%v)", err)
	}

	if !reflect.DeepEqual(card, expected) {
		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
	}
}

func TestUnmarshalCardXWithMissingCardNumber(t *testing.T) {
	from := date("2020-01-01")
	to := date("2020-12-31")

	expected := CardX{
		CardNumber: 0,
		From:       &from,
		To:         &to,
		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true},
	}

	blob := `{
  "start-date": "2020-01-01",
  "end-date": "2020-12-31",
  "doors": {
    "1":true,
    "2":false,
    "3":false,
    "4":true
  }
} `

	card := CardX{}
	err := json.Unmarshal([]byte(blob), &card)

	if err != nil {
		t.Errorf("Unexpected error unmarshalling card (%v)", err)
	}

	if !reflect.DeepEqual(card, expected) {
		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
	}
}

func TestUnmarshalCardXWithMissingStartDate(t *testing.T) {
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
	card := CardX{}
	err := json.Unmarshal([]byte(blob), &card)

	if err == nil {
		t.Errorf("Expected error unmarshalling card without start-date")
	}
}

func TestUnmarshalCardXWithMissingEndDate(t *testing.T) {
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
	card := CardX{}
	err := json.Unmarshal([]byte(blob), &card)

	if err == nil {
		t.Errorf("Expected error unmarshalling card without end-date")
	}
}

func TestUnmarshalCardXWithMissingDoors(t *testing.T) {
	from := date("2020-01-01")
	to := date("2020-12-31")

	expected := CardX{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors:      map[uint8]bool{1: false, 2: true, 3: false, 4: false},
	}

	blob := `{
  "card-number": 12345,
  "start-date": "2020-01-01",
  "end-date": "2020-12-31",
  "doors": {
    "2": true
  }
} `

	card := CardX{}
	err := json.Unmarshal([]byte(blob), &card)

	if err != nil {
		t.Fatalf("Unexpected error unmarshalling card (%v)", err)
	}

	if !reflect.DeepEqual(card, expected) {
		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
	}
}

func TestUnmarshalCardXWithInvalidDoors(t *testing.T) {
	from := date("2020-01-01")
	to := date("2020-12-31")

	expected := CardX{
		CardNumber: 12345,
		From:       &from,
		To:         &to,
		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: false},
	}

	blob := `{
  "card-number": 12345,
  "start-date": "2020-01-01",
  "end-date": "2020-12-31",
  "doors": {
    "1": true,
    "5": false
  }
} `
	card := CardX{}
	err := json.Unmarshal([]byte(blob), &card)

	if err != nil {
		t.Fatalf("Unexpected error unmarshalling card with invalid doors (%v)", err)
	}

	if !reflect.DeepEqual(card, expected) {
		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
	}
}
