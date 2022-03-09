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

func getDevice(uu uhppote.IUHPPOTE, deviceID uint32, d *C.struct_Device) error {
	if d == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to Device struct")
	}

	if DEBUG {
		fmt.Printf(">>> get-device\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	d.ID = C.ulong(deviceID)
	d.address = C.CString("192.168.1.101")
	d.subnet = C.CString("255.255.255.0")
	d.gateway = C.CString("192.168.1.1")
	d.MAC = C.CString("00:12:23:34:45:56")
	d.version = C.CString("v8.92")
	d.date = C.CString("2018-11-05")

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
