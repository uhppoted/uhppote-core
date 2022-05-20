package types

import (
	"encoding/json"
	"fmt"
)

type Card struct {
	CardNumber uint32          `json:"card-number"`
	From       *Date           `json:"start-date"`
	To         *Date           `json:"end-date"`
	Doors      map[uint8]uint8 `json:"doors"`
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

	from := "-         "
	if c.From != nil {
		from = fmt.Sprintf("%v", c.From)
	}

	to := "-         "
	if c.To != nil {
		to = fmt.Sprintf("%v", c.To)
	}

	return fmt.Sprintf("%-8v %v %v %v %v %v %v", c.CardNumber, from, to, f(c.Doors[1]), f(c.Doors[2]), f(c.Doors[3]), f(c.Doors[4]))
}

func (c *Card) UnmarshalJSON(bytes []byte) error {
	card := struct {
		CardNumber uint32        `json:"card-number"`
		From       string        `json:"start-date"`
		To         string        `json:"end-date"`
		Doors      map[uint8]int `json:"doors"`
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
	c.From = &from
	c.To = &to
	c.Doors = map[uint8]uint8{}

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
	}

	return card
}
