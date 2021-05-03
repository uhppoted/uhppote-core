package types

import (
	"encoding/json"
	"fmt"
)

type Card struct {
	CardNumber uint32               `json:"card-number"`
	From       *Date                `json:"start-date"`
	To         *Date                `json:"end-date"`
	Doors      map[uint8]Permission `json:"doors"`
}

type Permission interface{}

func (c Card) String() string {
	f := func(p Permission) string {
		if p != nil {
			switch v := p.(type) {
			case bool:
				if v {
					return "Y"
				}

			case uint8:
				return fmt.Sprintf("%v", v)
			}
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

	return fmt.Sprintf("%-8v %v %v %v %v %v %v", c.CardNumber, from, to, f(c.Doors[1]), f(c.Doors[2]), f(c.Doors[3]), f(c.Doors[4]))
}

func (c *Card) UnmarshalJSON(bytes []byte) error {
	card := struct {
		CardNumber uint32               `json:"card-number"`
		From       string               `json:"start-date"`
		To         string               `json:"end-date"`
		Doors      map[uint8]Permission `json:"doors"`
	}{
		Doors: map[uint8]Permission{1: false, 2: false, 3: false, 4: false},
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
	c.Doors = map[uint8]Permission{}

	for _, i := range []uint8{1, 2, 3, 4} {
		switch v := card.Doors[i].(type) {
		case bool:
			c.Doors[i] = v

		case int8:
			c.Doors[i] = uint8(v)

		case int16:
			c.Doors[i] = uint8(v)

		case int32:
			c.Doors[i] = uint8(v)

		case int64:
			c.Doors[i] = uint8(v)

		case uint8:
			c.Doors[i] = uint8(v)

		case uint16:
			c.Doors[i] = uint8(v)

		case uint32:
			c.Doors[i] = uint8(v)

		case uint64:
			c.Doors[i] = uint8(v)

		case float32:
			c.Doors[i] = uint8(v)

		case float64:
			c.Doors[i] = uint8(v)

		default:
			c.Doors[i] = false
		}
	}

	return nil
}

func (c *Card) Clone() Card {
	card := Card{
		CardNumber: c.CardNumber,
		From:       c.From,
		To:         c.To,
		Doors: map[uint8]Permission{
			1: c.Doors[1],
			2: c.Doors[2],
			3: c.Doors[3],
			4: c.Doors[4],
		},
	}

	return card
}
