package types

import (
//	"encoding/json"
//	"fmt"
//	"reflect"
//	"testing"
//	"time"
)

// func TestCardToString(t *testing.T) {
// 	expected := "12345    2020-01-01 2020-12-31 Y N N Y"
//
// 	from := date("2020-01-01")
// 	to := date("2020-12-31")
// 	card := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      []bool{true, false, false, true},
// 	}
//
// 	s := fmt.Sprintf("%v", card)
//
// 	if s != expected {
// 		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
// 	}
// }
//
// func TestCardToStringWithMissingFromDate(t *testing.T) {
// 	expected := "12345    -          2020-12-31 Y N N Y"
//
// 	to := date("2020-12-31")
// 	card := Card{
// 		CardNumber: 12345,
// 		To:         &to,
// 		Doors:      []bool{true, false, false, true},
// 	}
//
// 	s := fmt.Sprintf("%v", card)
//
// 	if s != expected {
// 		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
// 	}
// }
//
// func TestCardToStringWithMissingToDate(t *testing.T) {
// 	expected := "12345    2020-01-01 -          Y N N Y"
//
// 	from := date("2020-01-01")
// 	card := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		Doors:      []bool{true, false, false, true},
// 	}
//
// 	s := fmt.Sprintf("%v", card)
//
// 	if s != expected {
// 		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
// 	}
// }
//
// func TestCardToStringWithMissingDoors(t *testing.T) {
// 	expected := "12345    2020-01-01 2020-12-31 Y N N N"
//
// 	from := date("2020-01-01")
// 	to := date("2020-12-31")
// 	card := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      []bool{true, false},
// 	}
//
// 	s := fmt.Sprintf("%v", card)
//
// 	if s != expected {
// 		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
// 	}
// }
//
// func TestCardToStringWithExtraDoors(t *testing.T) {
// 	expected := "12345    2020-01-01 2020-12-31 Y N N Y"
//
// 	from := date("2020-01-01")
// 	to := date("2020-12-31")
// 	card := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      []bool{true, false, false, true, true, false},
// 	}
//
// 	s := fmt.Sprintf("%v", card)
//
// 	if s != expected {
// 		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
// 	}
// }
//
// func TestUnmarshalCard(t *testing.T) {
// 	from := date("2020-01-01")
// 	to := date("2020-12-31")
//
// 	expected := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      []bool{true, false, false, true},
// 	}
//
// 	blob := `{
//   "card-number": 12345,
//   "start-date": "2020-01-01",
//   "end-date": "2020-12-31",
//   "doors": [
//     true,
//     false,
//     false,
//     true
//   ]
// } `
//
// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)
//
// 	if err != nil {
// 		t.Errorf("Unexpected error unmarshalling card (%v)", err)
// 	}
//
// 	if !reflect.DeepEqual(card, expected) {
// 		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
// 	}
// }
//
// func TestUnmarshalCardWithMissingCardNumber(t *testing.T) {
// 	from := date("2020-01-01")
// 	to := date("2020-12-31")
//
// 	expected := Card{
// 		CardNumber: 0,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      []bool{true, false, false, true},
// 	}
//
// 	blob := `{
//   "start-date": "2020-01-01",
//   "end-date": "2020-12-31",
//   "doors": [
//     true,
//     false,
//     false,
//     true
//   ]
// } `
//
// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)
//
// 	if err != nil {
// 		t.Errorf("Unexpected error unmarshalling card (%v)", err)
// 	}
//
// 	if !reflect.DeepEqual(card, expected) {
// 		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
// 	}
// }
//
// func TestUnmarshalCardWithMissingStartDate(t *testing.T) {
// 	blob := `{
//   "card-number": 12345,
//   "end-date": "2020-12-31",
//   "doors": [
//     true,
//     false,
//     false,
//     true
//   ]
// } `
// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)
//
// 	if err == nil {
// 		t.Errorf("Expected error unmarshalling card without start-date")
// 	}
// }
//
// func TestUnmarshalCardWithMissingEndDate(t *testing.T) {
// 	blob := `{
//   "card-number": 12345,
//   "start-date": "2020-01-01",
//   "doors": [
//     true,
//     false,
//     false,
//     true
//   ]
// } `
// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)
//
// 	if err == nil {
// 		t.Errorf("Expected error unmarshalling card without end-date")
// 	}
// }
//
// func TestUnmarshalCardWithMissingDoors(t *testing.T) {
// 	from := date("2020-01-01")
// 	to := date("2020-12-31")
//
// 	expected := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      []bool{false, false, false, false},
// 	}
//
// 	blob := `{
//   "card-number": 12345,
//   "start-date": "2020-01-01",
//   "end-date": "2020-12-31"
// } `
//
// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)
//
// 	if err != nil {
// 		t.Errorf("Unexpected error unmarshalling card (%v)", err)
// 	}
//
// 	if !reflect.DeepEqual(card, expected) {
// 		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
// 	}
// }
//
// func TestUnmarshalCardWithInvalidDoors(t *testing.T) {
// 	blob := `{
//   "card-number": 12345,
//   "start-date": "2020-01-01",
//   "end-date": "2020-12-31",
//   "doors": [
//     true,
//     false
//   ]
// } `
// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)
//
// 	if err == nil {
// 		t.Errorf("Expected error unmarshalling card with invalid doors")
// 	}
// }
