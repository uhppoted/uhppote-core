//go:build tests

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

	*N = C.int(39)

	return nil
}
