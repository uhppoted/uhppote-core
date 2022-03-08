package main

import "C"

import (
	"unsafe"
)

func getDevices(u *C.struct_UHPPOTE, N *C.int, list *C.uint) *C.char {
	if N == nil {
		return C.CString("invalid argument (N) - expected valid pointer")
	}

	if list == nil {
		return C.CString("invalid argument (list) - expected valid pointer to list")
	}

	_, err := makeUHPPOTE(u)
	if err != nil {
		return C.CString(err.Error())
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

func getDevice(u *C.struct_UHPPOTE, deviceID uint32, d *C.struct_Device) *C.char {
	if d == nil {
		return C.CString("invalid argument (device) - expected valid pointer to Device struct")
	}

	_, err := makeUHPPOTE(u)
	if err != nil {
		return C.CString(err.Error())
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
