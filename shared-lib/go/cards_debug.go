//go:build debug

package main

import "C"

import (
	"fmt"

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
