//go:build debug

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

func getCardByIndex(uu uhppote.IUHPPOTE, card *C.struct_Card, deviceID uint32, index uint32) error {
	if card == nil {
		return fmt.Errorf("invalid argument (card) - expected valid pointer")
	}

	if DEBUG {
		fmt.Printf(">>> get-card-by-index\n")
		fmt.Printf("    ID:    %v\n", deviceID)
		fmt.Printf("    index: %v\n", index)
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

	if DEBUG {
		fmt.Printf(">>> put-card\n")
		fmt.Printf("    ID:            %v\n", deviceID)
		fmt.Printf("    card number:   %v\n", cardNumber)
		fmt.Printf("         from:     %v\n", types.Date(_from))
		fmt.Printf("         to:       %v\n", types.Date(_to))
		fmt.Printf("         doors[1]: %v\n", int(_doors[0]))
		fmt.Printf("         doors[2]: %v\n", int(_doors[1]))
		fmt.Printf("         doors[3]: %v\n", int(_doors[2]))
		fmt.Printf("         doors[4]: %v\n", int(_doors[3]))
		fmt.Println()
	}

	return nil
}

func deleteCard(uu uhppote.IUHPPOTE, deviceID uint32, cardNumber uint32) error {
	if DEBUG {
		fmt.Printf(">>> delete-card\n")
		fmt.Printf("    ID:   %v\n", deviceID)
		fmt.Printf("    card: %v\n", cardNumber)
		fmt.Println()
	}

	return nil
}

func deleteCards(uu uhppote.IUHPPOTE, deviceID uint32) error {
	if DEBUG {
		fmt.Printf(">>> delete-cards\n")
		fmt.Printf("    ID:   %v\n", deviceID)
		fmt.Println()
	}

	return nil
}

func getTimeProfile(uu uhppote.IUHPPOTE, profile *C.struct_TimeProfile, deviceID uint32, profileID uint8) error {
	if profile == nil {
		return fmt.Errorf("invalid argument (profile) - expected valid pointer")
	}

	if DEBUG {
		fmt.Printf(">>> get-time-profile\n")
		fmt.Printf("    ID:      %v\n", deviceID)
		fmt.Printf("    profile: %v\n", profileID)
		fmt.Println()
	}

	profile.ID = 29
	profile.linked = 71
	profile.from = C.CString("2022-02-01")
	profile.to = C.CString("2022-06-30")

	profile.monday = cbool(true)
	profile.tuesday = cbool(false)
	profile.wednesday = cbool(true)
	profile.thursday = cbool(true)
	profile.friday = cbool(false)
	profile.saturday = cbool(false)
	profile.sunday = cbool(true)

	profile.segment1start = C.CString("08:30")
	profile.segment1end = C.CString("11:30")
	profile.segment2start = C.CString("")
	profile.segment2end = C.CString("")
	profile.segment3start = C.CString("14:45")
	profile.segment3end = C.CString("")

	return nil
}
