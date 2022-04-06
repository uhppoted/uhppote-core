//go:build debug

package main

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

	if DEBUG {
		fmt.Printf(">>> get-cards\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	*N = C.int(39)

	return nil
}

func getCard(uu uhppote.IUHPPOTE, card *C.struct_Card, deviceID uint32, cardNumber uint32) error {
	if card == nil {
		return fmt.Errorf("invalid argument (card) - expected valid pointer")
	}

	if DEBUG {
		fmt.Printf(">>> get-card\n")
		fmt.Printf("    ID:   %v\n", deviceID)
		fmt.Printf("    card: %v\n", cardNumber)
		fmt.Println()
	}

	card.card_number = C.uint(8165538)
	card.from = C.CString("2022-01-01")
	card.to = C.CString("2022-12-31")

	doors := unsafe.Slice(card.doors, 4)

	doors[0] = 0
	doors[1] = 1
	doors[2] = 31
	doors[3] = 75

	return nil
}
