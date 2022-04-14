//go:build tests

package main

import (
	"C"
	"fmt"

	"github.com/uhppoted/uhppote-core/uhppote"
)

func getEventIndex(uu uhppote.IUHPPOTE, index *uint32, deviceID uint32) error {
	if index == nil {
		return fmt.Errorf("invalid argument (index) - expected valid pointer")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	*index = 47

	return nil
}

func setEventIndex(uu uhppote.IUHPPOTE, deviceID uint32, index uint32) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if index != 51 {
		return fmt.Errorf("Incorrect event index (%v)", index)
	}

	return nil
}
