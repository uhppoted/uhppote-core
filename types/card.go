package types

import (
	"encoding/json"
	"fmt"
)

type Authorised struct {
	SerialNumber uint32
	Authorised   bool
}

func (r *Authorised) String() string {
	return fmt.Sprintf("%v %v", r.SerialNumber, r.Authorised)
}

type Card struct {
	CardNumber uint32         `json:"card-number"`
	From       *Date          `json:"start-date"`
	To         *Date          `json:"end-date"`
	Doors      map[uint8]bool `json:"doors"`
}

func (c Card) String() string {
	f := func(d bool) string {
		if d {
			return "Y"
		}
		return "N"
	}

	from := "-         "
	if c.From != nil {
		from = fmt.Sprintf("%v", c.From)
	}

	to := "-         "
	if c.To != nil {
		to = fmt.Sprintf("%v", c.To)
	}

	return fmt.Sprintf("%-8v %v %v %s %s %s %s", c.CardNumber, from, to, f(c.Doors[1]), f(c.Doors[2]), f(c.Doors[3]), f(c.Doors[4]))
}

func (c *Card) UnmarshalJSON(bytes []byte) error {
	card := struct {
		CardNumber uint32         `json:"card-number"`
		From       string         `json:"start-date"`
		To         string         `json:"end-date"`
		Doors      map[uint8]bool `json:"doors"`
	}{
		Doors: map[uint8]bool{1: false, 2: false, 3: false, 4: false},
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

	doors := map[uint8]bool{}
	doors[1] = card.Doors[1]
	doors[2] = card.Doors[2]
	doors[3] = card.Doors[3]
	doors[4] = card.Doors[4]

	*c = Card{
		CardNumber: card.CardNumber,
		From:       from,
		To:         to,
		Doors:      doors,
	}

	return nil
}

func (c *Card) Clone() Card {
	card := Card{
		CardNumber: c.CardNumber,
		From:       c.From,
		To:         c.To,
		Doors:      map[uint8]bool{},
	}

	card.Doors[1] = c.Doors[1]
	card.Doors[2] = c.Doors[2]
	card.Doors[3] = c.Doors[3]
	card.Doors[4] = c.Doors[4]

	return card
}
