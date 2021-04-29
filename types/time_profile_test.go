package types

import ()

// func TestTimeProfileToString(t *testing.T) {
// 	expected := "12345    2020-01-01 2020-12-31 Y N N Y"

// 	from := date("2020-01-01")
// 	to := date("2020-12-31")
// 	card := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true},
// 	}

// 	s := fmt.Sprintf("%v", card)

// 	if s != expected {
// 		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
// 	}
// }

// func TestTimeProfileToStringWithMissingFromDate(t *testing.T) {
// 	expected := "12345    -          2020-12-31 Y N N Y"

// 	to := date("2020-12-31")
// 	card := Card{
// 		CardNumber: 12345,
// 		To:         &to,
// 		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true},
// 	}

// 	s := fmt.Sprintf("%v", card)

// 	if s != expected {
// 		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
// 	}
// }

// func TestProfileToStringWithMissingToDate(t *testing.T) {
// 	expected := "12345    2020-01-01 -          Y N N Y"

// 	from := date("2020-01-01")
// 	card := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true},
// 	}

// 	s := fmt.Sprintf("%v", card)

// 	if s != expected {
// 		t.Errorf("Card incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
// 	}
// }

// func TestUnmarshalCard(t *testing.T) {
// 	from := date("2020-01-01")
// 	to := date("2020-12-31")

// 	expected := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true},
// 	}

// 	blob := `{
//   "card-number": 12345,
//   "start-date": "2020-01-01",
//   "end-date": "2020-12-31",
//   "doors": {
//     "1": true,
//     "2": false,
//     "3": false,
//     "4": true
//   }
// } `

// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)

// 	if err != nil {
// 		t.Errorf("Unexpected error unmarshalling card (%v)", err)
// 	}

// 	if !reflect.DeepEqual(card, expected) {
// 		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
// 	}
// }

// func TestUnmarshalTimeProfileWithMissingCardNumber(t *testing.T) {
// 	from := date("2020-01-01")
// 	to := date("2020-12-31")

// 	expected := Card{
// 		CardNumber: 0,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: true},
// 	}

// 	blob := `{
//   "start-date": "2020-01-01",
//   "end-date": "2020-12-31",
//   "doors": {
//     "1":true,
//     "2":false,
//     "3":false,
//     "4":true
//   }
// } `

// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)

// 	if err != nil {
// 		t.Errorf("Unexpected error unmarshalling card (%v)", err)
// 	}

// 	if !reflect.DeepEqual(card, expected) {
// 		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
// 	}
// }

// func TestUnmarshalCardWithMissingStartDate(t *testing.T) {
// 	blob := `{
//   "card-number": 12345,
//   "end-date": "2020-12-31",
//   "doors": [
//     "1": true,
//     "2": false,
//     "3": false,
//     "4": true
//   ]
// } `
// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)

// 	if err == nil {
// 		t.Errorf("Expected error unmarshalling card without start-date")
// 	}
// }

// func TestUnmarshalCardWithMissingEndDate(t *testing.T) {
// 	blob := `{
//   "card-number": 12345,
//   "start-date": "2020-01-01",
//   "doors": [
//     "1": true,
//     "2": false,
//     "3": false,
//     "4": true
//   ]
// } `
// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)

// 	if err == nil {
// 		t.Errorf("Expected error unmarshalling card without end-date")
// 	}
// }

// func TestUnmarshalCardWithMissingDoors(t *testing.T) {
// 	from := date("2020-01-01")
// 	to := date("2020-12-31")

// 	expected := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      map[uint8]bool{1: false, 2: true, 3: false, 4: false},
// 	}

// 	blob := `{
//   "card-number": 12345,
//   "start-date": "2020-01-01",
//   "end-date": "2020-12-31",
//   "doors": {
//     "2": true
//   }
// } `

// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)

// 	if err != nil {
// 		t.Fatalf("Unexpected error unmarshalling card (%v)", err)
// 	}

// 	if !reflect.DeepEqual(card, expected) {
// 		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
// 	}
// }

// func TestUnmarshalCardWithInvalidDoors(t *testing.T) {
// 	from := date("2020-01-01")
// 	to := date("2020-12-31")

// 	expected := Card{
// 		CardNumber: 12345,
// 		From:       &from,
// 		To:         &to,
// 		Doors:      map[uint8]bool{1: true, 2: false, 3: false, 4: false},
// 	}

// 	blob := `{
//   "card-number": 12345,
//   "start-date": "2020-01-01",
//   "end-date": "2020-12-31",
//   "doors": {
//     "1": true,
//     "5": false
//   }
// } `
// 	card := Card{}
// 	err := json.Unmarshal([]byte(blob), &card)

// 	if err != nil {
// 		t.Fatalf("Unexpected error unmarshalling card with invalid doors (%v)", err)
// 	}

// 	if !reflect.DeepEqual(card, expected) {
// 		t.Errorf("Card incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, card)
// 	}
// }
