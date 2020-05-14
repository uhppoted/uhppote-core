package types

import (
	"encoding/json"
	"fmt"
)

type Card struct {
	CardNumber uint32 `json:"card-number"`
	From       *Date  `json:"start-date"`
	To         *Date  `json:"end-date"`
	Doors      []bool `json:"doors"`
}

type Authorised struct {
	SerialNumber uint32
	Authorised   bool
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

	doors := []bool{false, false, false, false}
	copy(doors, c.Doors)

	return fmt.Sprintf("%-8v %v %v %s %s %s %s", c.CardNumber, from, to, f(doors[0]), f(doors[1]), f(doors[2]), f(doors[3]))
}

func (c *Card) UnmarshalJSON(bytes []byte) error {
	card := struct {
		CardNumber uint32 `json:"card-number"`
		From       string `json:"start-date"`
		To         string `json:"end-date"`
		Doors      []bool `json:"doors"`
	}{}

	if err := json.Unmarshal(bytes, &card); err != nil {
		return err
	}

	from, err := DateFromString(card.From)
	if err != nil {
		return err
	}

	to, err := DateFromString(card.To)
	if err != nil {
		return err
	}

	doors := card.Doors
	if len(doors) == 0 {
		doors = []bool{false, false, false, false}
	} else if len(doors) != 4 {
		return fmt.Errorf("Expected values for 4 doors, got %v", doors)
	}

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
		Doors:      make([]bool, len(c.Doors)),
	}

	copy(card.Doors, c.Doors)

	return card
}

func (r *Authorised) String() string {
	return fmt.Sprintf("%v %v", r.SerialNumber, r.Authorised)
}
