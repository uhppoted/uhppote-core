package types

import (
	"encoding/json"
	"fmt"
)

type CardFormat uint8

const (
	WiegandAny CardFormat = iota
	Wiegand26
)

func (f CardFormat) String() string {
	return []string{"any", "Wiegand-26"}[f]
}

type Card struct {
	CardNumber uint32          `json:"card-number"`
	From       Date            `json:"start-date"`
	To         Date            `json:"end-date"`
	Doors      map[uint8]uint8 `json:"doors"`
	PIN        PIN             `json:"PIN,omitempty"`
}

func (c Card) String() string {
	f := func(p uint8) string {
		switch {
		case p == 0:
			return "N"

		case p == 1:
			return "Y"

		case p >= 2 && p <= 254:
			return fmt.Sprintf("%v", p)

		default:
			return "N"
		}
	}

	var from string
	if c.From.IsZero() {
		from = "-"
	} else {
		from = fmt.Sprintf("%v", c.From)
	}

	var to string
	if c.To.IsZero() {
		to = "-"
	} else {
		to = fmt.Sprintf("%v", c.To)
	}

	if c.PIN == 0 || c.PIN > 999999 {
		return fmt.Sprintf("%-8v %-10v %-10v %v %v %v %v", c.CardNumber, from, to, f(c.Doors[1]), f(c.Doors[2]), f(c.Doors[3]), f(c.Doors[4]))
	} else {
		return fmt.Sprintf("%-8v %-10v %-10v %v %v %v %v %v", c.CardNumber, from, to, f(c.Doors[1]), f(c.Doors[2]), f(c.Doors[3]), f(c.Doors[4]), c.PIN)
	}
}

func (c Card) MarshalJSON() ([]byte, error) {
	card := struct {
		CardNumber uint32          `json:"card-number"`
		From       Date            `json:"start-date"`
		To         Date            `json:"end-date"`
		Doors      map[uint8]uint8 `json:"doors"`
		PIN        PIN             `json:"PIN,omitempty"`
	}{
		CardNumber: c.CardNumber,
		From:       c.From,
		To:         c.To,
		Doors:      c.Doors,
		PIN:        c.PIN,
	}

	if card.PIN > 999999 {
		card.PIN = 0
	}

	return json.Marshal(card)
}

func (c *Card) UnmarshalJSON(bytes []byte) error {
	card := struct {
		CardNumber uint32        `json:"card-number"`
		From       string        `json:"start-date"`
		To         string        `json:"end-date"`
		Doors      map[uint8]int `json:"doors"`
		PIN        PIN           `json:"PIN"`
	}{
		Doors: map[uint8]int{1: 0, 2: 0, 3: 0, 4: 0},
	}

	if err := json.Unmarshal(bytes, &card); err != nil {
		return err
	}

	from, err := DateFromString(card.From)
	if err != nil {
		return fmt.Errorf("invalid start-date '%s'", card.From)
	}

	to, err := DateFromString(card.To)
	if err != nil {
		return fmt.Errorf("invalid end-date '%s'", card.To)
	}

	c.CardNumber = card.CardNumber
	c.From = from
	c.To = to
	c.Doors = map[uint8]uint8{}
	c.PIN = card.PIN

	for _, i := range []uint8{1, 2, 3, 4} {
		c.Doors[i] = uint8(card.Doors[i])
	}

	return nil
}

func (c *Card) Clone() Card {
	card := Card{
		CardNumber: c.CardNumber,
		From:       c.From,
		To:         c.To,
		Doors: map[uint8]uint8{
			1: c.Doors[1],
			2: c.Doors[2],
			3: c.Doors[3],
			4: c.Doors[4],
		},
		PIN: c.PIN,
	}

	return card
}
