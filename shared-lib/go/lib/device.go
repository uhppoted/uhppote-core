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

	devices, err := uu.GetDevices()
	if err != nil {
		return err
	}

	slice := unsafe.Slice(list, *N)
	for ix, device := range devices {
		if ix < int(*N) {
			slice[ix] = C.uint(device.SerialNumber)
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

	device, err := uu.GetDevice(deviceID)
	if err != nil {
		return err
	}

	if device == nil {
		return fmt.Errorf("No device found for %v", deviceID)
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

	if result, err := uu.SetAddress(deviceID, _address, _subnet, _gateway); err != nil {
		return err
	} else if result == nil {
		return fmt.Errorf("invalid reply from device (%v)", result)
	} else if !result.Succeeded {
		return fmt.Errorf("failed to set device address")
	}

	return nil
}
