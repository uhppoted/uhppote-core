package types

import (
	"encoding/json"
	"fmt"
	"strings"
)

type Card struct {
	CardNumber uint32          `json:"card-number"`
	From       Date            `json:"start-date"`
	To         Date            `json:"end-date"`
	Doors      map[uint8]uint8 `json:"doors"`
	PIN        PIN             `json:"PIN,omitempty"`
	FirstCard  map[uint8]bool  `json:"first-card"`
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

	g := func(d uint8) string {
		if v, ok := c.FirstCard[d]; v && ok {
			return "Y"
		} else {
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

	firstcard := strings.Join([]string{g(1), g(2), g(3), g(4)}, ",")

	if c.PIN == 0 || c.PIN > 999999 {
		return fmt.Sprintf("%-8v %-10v %-10v %v %v %v %v  %v", c.CardNumber, from, to, f(c.Doors[1]), f(c.Doors[2]), f(c.Doors[3]), f(c.Doors[4]), firstcard)
	} else {
		return fmt.Sprintf("%-8v %-10v %-10v %v %v %v %v %v  %v", c.CardNumber, from, to, f(c.Doors[1]), f(c.Doors[2]), f(c.Doors[3]), f(c.Doors[4]), c.PIN, firstcard)
	}
}

func (c Card) MarshalJSON() ([]byte, error) {
	card := struct {
		CardNumber uint32          `json:"card-number"`
		From       Date            `json:"start-date"`
		To         Date            `json:"end-date"`
		Doors      map[uint8]uint8 `json:"doors"`
		PIN        PIN             `json:"PIN,omitempty"`
		FirstCard  map[uint8]bool  `json:"first-card,omitempty"`
	}{
		CardNumber: c.CardNumber,
		From:       c.From,
		To:         c.To,
		Doors:      c.Doors,
		PIN:        c.PIN,
		FirstCard:  c.FirstCard,
	}

	if card.PIN > 999999 {
		card.PIN = 0
	}

	return json.Marshal(card)
}

func (c *Card) UnmarshalJSON(bytes []byte) error {
	card := struct {
		CardNumber uint32         `json:"card-number"`
		From       string         `json:"start-date"`
		To         string         `json:"end-date"`
		Doors      map[uint8]int  `json:"doors"`
		PIN        PIN            `json:"PIN"`
		FirstCard  map[uint8]bool `json:"first-card"`
	}{
		Doors:     map[uint8]int{1: 0, 2: 0, 3: 0, 4: 0},
		FirstCard: map[uint8]bool{1: false, 2: false, 3: false, 4: false},
	}

	if err := json.Unmarshal(bytes, &card); err != nil {
		return err
	}

	from, err := ParseDate(card.From)
	if err != nil {
		return fmt.Errorf("invalid start-date '%s'", card.From)
	}

	to, err := ParseDate(card.To)
	if err != nil {
		return fmt.Errorf("invalid end-date '%s'", card.To)
	}

	c.CardNumber = card.CardNumber
	c.From = from
	c.To = to
	c.Doors = map[uint8]uint8{
		1: 0,
		2: 0,
		3: 0,
		4: 0,
	}
	c.PIN = card.PIN
	c.FirstCard = map[uint8]bool{
		1: false,
		2: false,
		3: false,
		4: false,
	}

	for _, i := range []uint8{1, 2, 3, 4} {
		c.Doors[i] = uint8(card.Doors[i])
	}

	for _, i := range []uint8{1, 2, 3, 4} {
		c.FirstCard[i] = card.FirstCard[i]
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
