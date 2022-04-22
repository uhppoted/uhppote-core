//go:build tests

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

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	*N = C.int(39)

	return nil
}

func getCard(uu uhppote.IUHPPOTE, card *C.struct_Card, deviceID uint32, cardNumber uint32) error {
	if card == nil {
		return fmt.Errorf("invalid argument (card) - expected valid pointer")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if cardNumber != 8165538 {
		return fmt.Errorf("Incorrect card number (%v)", cardNumber)
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

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if index != 19 {
		return fmt.Errorf("Incorrect card index (%v)", index)
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
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if cardNumber != 8165538 {
		return fmt.Errorf("Incorrect card number (%v)", cardNumber)
	}

	if _from, err := time.Parse("2006-01-02", C.GoString(from)); err != nil {
		return fmt.Errorf("Invalid 'from' date (%v)", err)
	} else if fmt.Sprintf("%v", types.Date(_from)) != "2022-01-01" {
		return fmt.Errorf("Incorrect 'from' date (%v)", from)
	}

	if _to, err := time.Parse("2006-01-02", C.GoString(to)); err != nil {
		return fmt.Errorf("Invalid 'to' date (%v)", err)
	} else if fmt.Sprintf("%v", types.Date(_to)) != "2022-12-31" {
		return fmt.Errorf("Incorrect 'to' date (%v)", to)
	}

	if doors == nil {
		return fmt.Errorf("invalid argument (doors) - expected valid pointer")
	}

	_doors := unsafe.Slice(doors, 4)
	for i := 0; i < 4; i++ {
		if _doors[i] != []uint8{0, 1, 31, 75}[i] {
			return fmt.Errorf("Incorrect doors[%v] (%v)", i+1, _doors[i])
		}
	}

	return nil
}

func deleteCard(uu uhppote.IUHPPOTE, deviceID uint32, cardNumber uint32) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if cardNumber != 8165538 {
		return fmt.Errorf("Incorrect card number (%v)", cardNumber)
	}

	return nil
}

func deleteCards(uu uhppote.IUHPPOTE, deviceID uint32) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	return nil
}

func getTimeProfile(uu uhppote.IUHPPOTE, profile *C.struct_TimeProfile, deviceID uint32, profileID uint8) error {
	if profile == nil {
		return fmt.Errorf("invalid argument (profile) - expected valid pointer")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if profileID != 49 {
		return fmt.Errorf("Incorrect profile ID (%v)", profileID)
	}

	profile.ID = 49
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

func setTimeProfile(uu uhppote.IUHPPOTE, deviceID uint32, profile *C.struct_TimeProfile) error {
	if profile == nil {
		return fmt.Errorf("invalid argument (profile) - expected valid pointer")
	}

	p, err := makeTimeProfile(*profile)
	if err != nil {
		return err
	} else if p == nil {
		return fmt.Errorf("invalid time profile (%v)", p)
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if p.ID != 49 {
		return fmt.Errorf("Incorrect profile ID (%v)", p.ID)
	}

	if p.LinkedProfileID != 71 {
		return fmt.Errorf("Incorrect linked profile ID (%v)", p.LinkedProfileID)
	}

	if from := fmt.Sprintf("%v", p.From); from != "2022-02-01" {
		return fmt.Errorf("Incorrect 'from' date (%v)", from)
	}

	if to := fmt.Sprintf("%v", p.To); to != "2022-06-30" {
		return fmt.Errorf("Incorrect 'to' date (%v)", to)
	}

	if !p.Weekdays[time.Monday] {
		return fmt.Errorf("Incorrect Monday value (%v)", p.Weekdays[time.Monday])
	}

	if p.Weekdays[time.Tuesday] {
		return fmt.Errorf("Incorrect Tuesday value (%v)", p.Weekdays[time.Tuesday])
	}

	if !p.Weekdays[time.Wednesday] {
		return fmt.Errorf("Incorrect Wednesday value (%v)", p.Weekdays[time.Wednesday])
	}

	if !p.Weekdays[time.Thursday] {
		return fmt.Errorf("Incorrect Thursday value (%v)", p.Weekdays[time.Thursday])
	}

	if p.Weekdays[time.Friday] {
		return fmt.Errorf("Incorrect Friday value (%v)", p.Weekdays[time.Friday])
	}

	if p.Weekdays[time.Saturday] {
		return fmt.Errorf("Incorrect Saturday value (%v)", p.Weekdays[time.Saturday])
	}

	if !p.Weekdays[time.Sunday] {
		return fmt.Errorf("Incorrect Sunday value (%v)", p.Weekdays[time.Sunday])
	}

	if start := fmt.Sprintf("%v", p.Segments[1].Start); start != "08:30" {
		return fmt.Errorf("Incorrect segment 1 start (%v)", start)
	}

	if end := fmt.Sprintf("%v", p.Segments[1].End); end != "11:30" {
		return fmt.Errorf("Incorrect segment 1 end (%v)", end)
	}

	if start := fmt.Sprintf("%v", p.Segments[2].Start); start != "00:00" {
		return fmt.Errorf("Incorrect segment 2 start (%v)", start)
	}

	if end := fmt.Sprintf("%v", p.Segments[2].End); end != "00:00" {
		return fmt.Errorf("Incorrect segment 2 end (%v)", end)
	}

	if start := fmt.Sprintf("%v", p.Segments[3].Start); start != "00:00" {
		return fmt.Errorf("Incorrect segment 3 start (%v)", start)
	}

	if end := fmt.Sprintf("%v", p.Segments[3].End); end != "18:00" {
		return fmt.Errorf("Incorrect segment 3 end (%v)", end)
	}

	return nil
}
