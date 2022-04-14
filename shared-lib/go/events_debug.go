//go:build debug

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

	if DEBUG {
		fmt.Printf(">>> get-event-index\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	*index = 73

	return nil
}

func setEventIndex(uu uhppote.IUHPPOTE, deviceID uint32, index uint32) error {
	if DEBUG {
		fmt.Printf(">>> set-event-index\n")
		fmt.Printf("    ID:    %v\n", deviceID)
		fmt.Printf("    index: %v\n", index)
		fmt.Println()
	}

	return nil
}
