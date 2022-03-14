package main

import "C"

import (
	"fmt"
	"net"
	"unsafe"

	"github.com/uhppoted/uhppote-core/uhppote"
)

func getDevices(uu uhppote.IUHPPOTE, N *C.int, list *C.uint) error {
	if N == nil {
		return fmt.Errorf("invalid argument (N) - expected valid pointer")
	}

	if list == nil {
		return fmt.Errorf("invalid argument (list) - expected valid pointer to list")
	}

	if DEBUG {
		fmt.Printf(">>> get-devices\n")
		fmt.Printf("    N:  %v\n", *N)
		fmt.Println()
	}

	devices := []uint32{201020304, 303986753, 405419896}

	slice := unsafe.Slice(list, *N)
	for ix, device := range devices {
		if ix < int(*N) {
			slice[ix] = C.uint(device)
		} else {
			break
		}
	}

	*N = C.int(len(devices))

	return nil
}

func getDevice(uu uhppote.IUHPPOTE, deviceID uint32, device *C.struct_Device) error {
	if device == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to Device struct")
	}

	if DEBUG {
		fmt.Printf(">>> get-device\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	device.ID = C.ulong(deviceID)
	device.address = C.CString("192.168.1.101")
	device.subnet = C.CString("255.255.255.0")
	device.gateway = C.CString("192.168.1.1")
	device.MAC = C.CString("00:12:23:34:45:56")
	device.version = C.CString("v8.92")
	device.date = C.CString("2018-11-05")

	return nil
}

func setAddress(uu uhppote.IUHPPOTE, deviceID uint32, address, subnet, gateway *C.char) error {
	_address := net.ParseIP(C.GoString(address))
	if _address == nil {
		return fmt.Errorf("invalid IP address (%v)", C.GoString(address))
	}

	_subnet := net.ParseIP(C.GoString(subnet))
	if _subnet == nil {
		return fmt.Errorf("invalid IP subnet mask (%v)", C.GoString(subnet))
	}

	_gateway := net.ParseIP(C.GoString(gateway))
	if _gateway == nil {
		return fmt.Errorf("invalid IP gateway address (%v)", C.GoString(gateway))
	}

	if DEBUG {
		fmt.Printf(">>> set-address\n")
		fmt.Printf("    ID:      %v\n", deviceID)
		fmt.Printf("    address: %v\n", _address)
		fmt.Printf("    subnet:  %v\n", _subnet)
		fmt.Printf("    gateway: %v\n", _gateway)
		fmt.Println()
	}

	return nil
}

func getStatus(uu uhppote.IUHPPOTE, deviceID uint32, status *C.struct_Status) error {
	if status == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
	}

	if status.event == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status.Event struct")
	}

	if DEBUG {
		fmt.Printf(">>> get-status\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	status.ID = C.ulong(deviceID)
	status.sysdatetime = C.CString("2022-03-19 15:48:32")

	doors := unsafe.Slice(status.doors, 4)
	buttons := unsafe.Slice(status.buttons, 4)

	doors[0] = 1
	doors[1] = 0
	doors[2] = 0
	doors[3] = 1

	buttons[0] = 1
	buttons[1] = 0
	buttons[2] = 1
	buttons[3] = 0

	status.relays = 0x12
	status.inputs = 0x34

	status.syserror = 0x56
	status.info = 253
	status.seqno = 9876

	status.event.timestamp = C.CString("2022-01-02 12:34:56")
	status.event.index = 135
	status.event.eventType = 0x06
	status.event.granted = 1
	status.event.door = 3
	status.event.direction = 1
	status.event.card = 8100023
	status.event.reason = 0x15

	return nil
}
