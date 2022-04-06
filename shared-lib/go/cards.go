//go:build !debug && !tests

package main

import "C"
import (
	"C"
	"fmt"
	"unsafe"

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

	response, err := uu.GetCardByID(deviceID, cardNumber)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-card %v", deviceID, cardNumber)
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
