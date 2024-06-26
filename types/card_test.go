package types

import (
	"encoding/json"
	"fmt"
	"reflect"
	"testing"
)

func TestCardToString(t *testing.T) {
	expected := "12345    2020-01-01 2020-12-31 Y N 29 Y"

	card := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
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

func TestCardWithPINToString(t *testing.T) {
	expected := "12345    2020-01-01 2020-12-31 Y N 29 Y 987654"

	card := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
			1: 1,
			2: 0,
			3: 29,
			4: 1,
		},
		PIN: 987654,
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestCardWithInvalidPINToString(t *testing.T) {
	expected := "12345    2020-01-01 2020-12-31 Y N 29 Y"

	card := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
			1: 1,
			2: 0,
			3: 29,
			4: 1,
		},
		PIN: 1000000,
	}

	s := fmt.Sprintf("%v", card)

	if s != expected {
		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestCardToStringWithMissingFromDate(t *testing.T) {
	expected := "12345    -          2020-12-31 Y N N Y"

	card := Card{
		CardNumber: 12345,
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
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

	from := MustParseDate("2020-01-01")
	card := Card{
		CardNumber: 12345,
		From:       from,
		Doors: map[uint8]uint8{
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

	card := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
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

	card := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
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

func TestCardMarshalJSON(t *testing.T) {

	card := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
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

func TestCardWithPINMarshalJSON(t *testing.T) {
	card := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
			1: 1,
			2: 0,
			3: 29,
			4: 1,
		},
		PIN: 987654,
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
  },
  "PIN": "987654"
}`

	blob, err := json.MarshalIndent(card, "", "  ")
	if err != nil {
		t.Errorf("Unexpected error unmarshalling card (%v)", err)
	}

	if !reflect.DeepEqual(string(blob), expected) {
		t.Errorf("Card incorrectly unmarshalled\n   expected:%v\n   got:     %v", expected, string(blob))

	}
}

func TestCardWithInvalidPINMarshalJSON(t *testing.T) {
	card := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
			1: 1,
			2: 0,
			3: 29,
			4: 1,
		},
		PIN: 98765432,
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
		t.Errorf("Unexpected error marshalling card (%v)", err)
	}

	if !reflect.DeepEqual(string(blob), expected) {
		t.Errorf("Card incorrectly marshalled\n   expected:%v\n   got:     %v", expected, string(blob))

	}
}

func TestUnmarshalCard(t *testing.T) {
	expected := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
			1: 1,
			2: 0,
			3: 29,
			4: 1,
		},
		PIN: 0,
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

func TestUnmarshalCardWithPIN(t *testing.T) {
	expected := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
			1: 1,
			2: 0,
			3: 29,
			4: 1,
		},
		PIN: 987654,
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
  },
  "PIN": "987654"
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
	expected := Card{
		CardNumber: 0,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
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
	expected := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
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
	expected := Card{
		CardNumber: 12345,
		From:       MustParseDate("2020-01-01"),
		To:         MustParseDate("2020-12-31"),
		Doors: map[uint8]uint8{
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

func TestUnmarshalCardWithInvalidPIN(t *testing.T) {
	blob := `{
  "card-number": 12345,
  "start-date": "2023-01-01",
  "end-date": "2023-12-31",
  "doors": {
    "1": 1,
    "2": 0,
    "3": 29,
    "4": 1
  },
  "PIN": "1000000"
} `

	card := Card{}

	if err := json.Unmarshal([]byte(blob), &card); err == nil {
		t.Errorf("Expected error unmarshalling card with invalid PIN, got:%v", err)
	}
}
