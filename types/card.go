package types

import (
	"encoding/json"
	"fmt"
	"strings"
)

type Card struct {
	CardNumber uint32              `json:"card-number"`
	From       Date                `json:"start-date"`
	To         Date                `json:"end-date"`
	Doors      map[uint8]uint8     `json:"doors"`
	PIN        PIN                 `json:"PIN,omitempty"`
	FirstCard  FirstCardPrivileges `json:"first-card"`
}

type FirstCardPrivileges struct {
	Door1 bool
	Door2 bool
	Door3 bool
	Door4 bool
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

	firstcard := ""
	if !c.FirstCard.IsZero() {
		firstcard = fmt.Sprintf(" firstcard:%v", c.FirstCard)
	}

	if c.PIN == 0 || c.PIN > 999999 {
		return fmt.Sprintf("%-8v %-10v %-10v %v %v %v %v%v", c.CardNumber, from, to, f(c.Doors[1]), f(c.Doors[2]), f(c.Doors[3]), f(c.Doors[4]), firstcard)
	} else {
		return fmt.Sprintf("%-8v %-10v %-10v %v %v %v %v %v%v", c.CardNumber, from, to, f(c.Doors[1]), f(c.Doors[2]), f(c.Doors[3]), f(c.Doors[4]), c.PIN, firstcard)
	}
}

func (c Card) MarshalJSON() ([]byte, error) {
	card := struct {
		CardNumber uint32              `json:"card-number"`
		From       Date                `json:"start-date"`
		To         Date                `json:"end-date"`
		Doors      map[uint8]uint8     `json:"doors"`
		PIN        PIN                 `json:"PIN,omitempty"`
		FirstCard  FirstCardPrivileges `json:"first-card,omitempty"`
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
		CardNumber uint32              `json:"card-number"`
		From       string              `json:"start-date"`
		To         string              `json:"end-date"`
		Doors      map[uint8]int       `json:"doors"`
		PIN        PIN                 `json:"PIN"`
		FirstCard  FirstCardPrivileges `json:"first-card"`
	}{
		Doors: map[uint8]int{1: 0, 2: 0, 3: 0, 4: 0},
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
	c.FirstCard = card.FirstCard

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
		PIN:       c.PIN,
		FirstCard: c.FirstCard,
	}

	return card
}

func (f FirstCardPrivileges) ForEach(fn func(door uint8, enabled bool)) {
	if fn != nil {
		fn(1, f.Door1)
		fn(2, f.Door2)
		fn(3, f.Door3)
		fn(4, f.Door4)
	}
}

func (f FirstCardPrivileges) IsZero() bool {
	isZero := true

	f.ForEach(func(d uint8, enabled bool) {
		if enabled {
			isZero = false
		}
	})

	return isZero
}

func (f FirstCardPrivileges) String() string {
	v := []string{}

	f.ForEach(func(d uint8, enabled bool) {
		if enabled {
			v = append(v, fmt.Sprintf("%v", d))
		}
	})

	if len(v) == 0 {
		return ""
	}

	return strings.Join(v, ",")
}

func (f FirstCardPrivileges) MarshalJSON() ([]byte, error) {
	m := map[uint8]bool{
		1: f.Door1,
		2: f.Door2,
		3: f.Door3,
		4: f.Door4,
	}

	return json.Marshal(m)
}

func (f *FirstCardPrivileges) UnmarshalJSON(bytes []byte) error {
	m := map[uint8]bool{
		1: false,
		2: false,
		3: false,
		4: false,
	}

	if err := json.Unmarshal(bytes, &m); err != nil {
		return err
	}

	f.Door1 = m[1]
	f.Door2 = m[2]
	f.Door3 = m[3]
	f.Door4 = m[4]

	return nil
}
