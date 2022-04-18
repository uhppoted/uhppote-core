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

func getEvent(uu uhppote.IUHPPOTE, event *C.struct_Event, deviceID uint32, index uint32) error {
	if event == nil {
		return fmt.Errorf("invalid argument (event) - expected valid pointer")
	}

	if DEBUG {
		fmt.Printf(">>> get-event\n")
		fmt.Printf("    ID:    %v\n", deviceID)
		fmt.Printf("    index: %v\n", index)
		fmt.Println()
	}

	event.timestamp = C.CString("2022-04-15 12:29:15")
	event.index = C.uint(index)
	event.eventType = C.uchar(0x06)
	event.granted = cbool(true)
	event.door = C.uchar(3)
	event.direction = C.uchar(1)
	event.card = C.uint(8165538)
	event.reason = C.uchar(0x15)

	return nil
}

func recordSpecialEvents(uu uhppote.IUHPPOTE, deviceID uint32, enabled bool) error {
	if DEBUG {
		fmt.Printf(">>> record-special-events\n")
		fmt.Printf("    ID:      %v\n", deviceID)
		fmt.Printf("    enabled: %v\n", enabled)
		fmt.Println()
	}

	return nil
}
