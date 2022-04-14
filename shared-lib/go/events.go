//go:build !debug && !tests

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

	reply, err := uu.GetEventIndex(deviceID)
	if err != nil {
		return err
	}

	*index = reply.Index

	return nil
}

func setEventIndex(uu uhppote.IUHPPOTE, deviceID uint32, index uint32) error {
	reply, err := uu.SetEventIndex(deviceID, index)
	if err != nil {
		return err
	} else if !reply.Changed {
		return fmt.Errorf("%v: failed to set event index", deviceID)
	}

	return nil
}
