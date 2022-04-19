//go:build tests

package main

import (
	"C"
	"fmt"
	"net"
	"time"
	"unsafe"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func getDevices(uu uhppote.IUHPPOTE, N *C.int, list *C.uint) error {
	if N == nil {
		return fmt.Errorf("invalid argument (N) - expected valid pointer")
	}

	if list == nil {
		return fmt.Errorf("invalid argument (list) - expected valid pointer to list")
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

func getDevice(uu uhppote.IUHPPOTE, device *C.struct_Device, deviceID uint32) error {
	if device == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to Device struct")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	device.ID = C.uint(deviceID)
	device.address = C.CString("192.168.1.101")
	device.subnet = C.CString("255.255.255.0")
	device.gateway = C.CString("192.168.1.1")
	device.MAC = C.CString("00:12:23:34:45:56")
	device.version = C.CString("v8.92")
	device.date = C.CString("2018-11-05")

	return nil
}

func setAddress(uu uhppote.IUHPPOTE, deviceID uint32, address, subnet, gateway *C.char) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	_address := C.GoString(address)
	if net.ParseIP(_address) == nil {
		return fmt.Errorf("invalid IP address (%v)", _address)
	} else if _address != "192.168.1.125" {
		return fmt.Errorf("Incorrect address (%v)", _address)
	}

	_subnet := C.GoString(subnet)
	if net.ParseIP(_subnet) == nil {
		return fmt.Errorf("invalid IP subnet mask (%v)", _subnet)
	} else if _subnet != "255.255.254.0" {
		return fmt.Errorf("Incorrect subnet mask (%v)", _subnet)
	}

	_gateway := C.GoString(gateway)
	if net.ParseIP(_gateway) == nil {
		return fmt.Errorf("invalid IP gateway address (%v)", _gateway)
	} else if _gateway != "192.168.1.0" {
		return fmt.Errorf("Incorrect gateway address (%v)", _gateway)
	}

	return nil
}

func getStatus(uu uhppote.IUHPPOTE, status *C.struct_Status, deviceID uint32) error {
	if status == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
	}

	if status.event == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status.Event struct")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	status.ID = C.uint(deviceID)
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

func getTime(uu uhppote.IUHPPOTE, datetime **C.char, deviceID uint32) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to string")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	*datetime = C.CString("2022-01-02 12:34:56")

	return nil
}
func setTime(uu uhppote.IUHPPOTE, deviceID uint32, datetime *C.char) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to string")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	t := C.GoString(datetime)
	if _, err := time.Parse("2006-01-02 15:04:05", t); err != nil {
		return err
	} else if t != "2022-03-23 12:24:17" {
		return fmt.Errorf("Incorrect date/time (%v)", t)
	}

	return nil
}

func getListener(uu uhppote.IUHPPOTE, address **C.char, deviceID uint32) error {
	if address == nil {
		return fmt.Errorf("invalid argument (address) - expected valid pointer to string")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	*address = C.CString("192.168.1.100:60001")

	return nil
}

func setListener(uu uhppote.IUHPPOTE, deviceID uint32, listener *C.char) error {
	if listener == nil {
		return fmt.Errorf("invalid argument (listener) - expected valid pointer to string")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	_listener := C.GoString(listener)
	if address, err := net.ResolveUDPAddr("udp", _listener); err != nil {
		return err
	} else if address == nil || address.IP.To4() == nil {
		return fmt.Errorf("Invalid UDP address: %v", listener)
	} else if _listener != "192.168.1.100:60001" {
		return fmt.Errorf("Incorrect listener address (%v)", _listener)
	}

	return nil
}

func getDoorControl(uu uhppote.IUHPPOTE, control *C.struct_DoorControl, deviceID uint32, door uint8) error {
	if control == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to DoorControl struct")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if door != 4 {
		return fmt.Errorf("Incorrect door (%v)", door)
	}

	control.mode = C.uchar(types.Controlled)
	control.delay = C.uchar(7)

	return nil
}

func setDoorControl(uu uhppote.IUHPPOTE, deviceID uint32, door uint8, mode types.ControlState, delay uint8) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if door != 4 {
		return fmt.Errorf("Incorrect door (%v)", door)
	}

	if mode != types.NormallyClosed {
		return fmt.Errorf("Incorrect door control mode (%v)", mode)
	}

	if delay != 6 {
		return fmt.Errorf("Incorrect door delay (%v)", delay)
	}

	return nil
}

func openDoor(uu uhppote.IUHPPOTE, deviceID uint32, door uint8) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if door != 4 {
		return fmt.Errorf("Incorrect door (%v)", door)
	}

	return nil
}
