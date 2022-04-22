//go:build !debug && !tests

package main

import (
	"C"
	"fmt"
	"time"
	"unsafe"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func getCards(uu uhppote.IUHPPOTE, N *C.int, deviceID uint32) error {
	if N == nil {
		return fmt.Errorf("invalid argument (N) - expected valid pointer")
	}

	cards, err := uu.GetCards(deviceID)
	if err != nil {
		return err
	}

	*N = C.int(cards)

	return nil
}

func getCard(uu uhppote.IUHPPOTE, card *C.struct_Card, deviceID uint32, cardNumber uint32) error {
	if card == nil {
		return fmt.Errorf("invalid argument (card) - expected valid pointer")
	}

	c, err := uu.GetCardByID(deviceID, cardNumber)
	if err != nil {
		return err
	} else if c == nil {
		return fmt.Errorf("%v: no response to get-card %v", deviceID, cardNumber)
	}

	card.card_number = C.uint(c.CardNumber)
	card.from = C.CString(fmt.Sprintf("%v", c.From))
	card.to = C.CString(fmt.Sprintf("%v", c.To))

	doors := unsafe.Slice(card.doors, 4)

	doors[0] = C.uchar(c.Doors[1])
	doors[1] = C.uchar(c.Doors[2])
	doors[2] = C.uchar(c.Doors[3])
	doors[3] = C.uchar(c.Doors[4])

	return nil
}

func getCardByIndex(uu uhppote.IUHPPOTE, card *C.struct_Card, deviceID uint32, index uint32) error {
	if card == nil {
		return fmt.Errorf("invalid argument (card) - expected valid pointer")
	}

	response, err := uu.GetCardByIndex(deviceID, index)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-card-by-index %v", deviceID, index)
	}

	card.card_number = C.uint(response.CardNumber)
	card.from = C.CString(fmt.Sprintf("%v", response.From))
	card.to = C.CString(fmt.Sprintf("%v", response.To))

	doors := unsafe.Slice(card.doors, 4)

	doors[0] = C.uchar(response.Doors[1])
	doors[1] = C.uchar(response.Doors[2])
	doors[2] = C.uchar(response.Doors[3])
	doors[3] = C.uchar(response.Doors[4])

	return nil
}

func putCard(uu uhppote.IUHPPOTE, deviceID uint32, cardNumber uint32, from, to *C.char, doors *uint8) error {
	_from, err := time.Parse("2006-01-02", C.GoString(from))
	if err != nil {
		return fmt.Errorf("Invalid 'from' date (%v)", err)
	}

	_to, err := time.Parse("2006-01-02", C.GoString(to))
	if err != nil {
		return fmt.Errorf("Invalid 'to' date (%v)", err)
	}

	if doors == nil {
		return fmt.Errorf("invalid argument (doors) - expected valid pointer")
	}

	_doors := unsafe.Slice(doors, 4)

	card := types.Card{
		CardNumber: cardNumber,
		From:       (*types.Date)(&_from),
		To:         (*types.Date)(&_to),
		Doors: map[uint8]int{
			1: int(_doors[0]),
			2: int(_doors[1]),
			3: int(_doors[2]),
			4: int(_doors[3]),
		},
	}

	ok, err := uu.PutCard(deviceID, card)
	if err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: put-card failed", deviceID)
	}

	return nil
}

func deleteCard(uu uhppote.IUHPPOTE, deviceID uint32, cardNumber uint32) error {
	deleted, err := uu.DeleteCard(deviceID, cardNumber)
	if err != nil {
		return err
	} else if !deleted {
		return fmt.Errorf("%v: delete-card %v failed", deviceID, cardNumber)
	}

	return nil
}

func deleteCards(uu uhppote.IUHPPOTE, deviceID uint32) error {
	deleted, err := uu.DeleteCards(deviceID)
	if err != nil {
		return err
	} else if !deleted {
		return fmt.Errorf("%v: delete-cards failed", deviceID)
	}

	return nil
}
