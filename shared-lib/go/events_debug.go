//go:build debug

package main

import (
	"C"
	"fmt"

	"github.com/uhppoted/uhppote-core/uhppote"
)

func getEventIndex(uu uhppote.IUHPPOTE, index *C.int, deviceID uint32) error {
	if index == nil {
		return fmt.Errorf("invalid argument (index) - expected valid pointer")
	}

	if DEBUG {
		fmt.Printf(">>> get-event-index\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	*index = C.int(73)

	return nil
}
