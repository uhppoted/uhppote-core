package main

import "C"

import (
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

func getStatus(uu uhppote.IUHPPOTE, deviceID uint32, status *C.struct_Status) error {
	if status == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
	}

	s, err := uu.GetStatus(deviceID)
	if err != nil {
		return err
	} else if s == nil {
		return fmt.Errorf("No status returned for %v", deviceID)
	}

	cbool := func(b bool) C.uchar {
		if b {
			return 1
		} else {
			return 0
		}
	}

	format := func(t *types.DateTime) string {
		if t != nil {
			return time.Time(*t).Format("2006-01-02 15:04:05")
		}

		return ""
	}

	status.ID = C.ulong(s.SerialNumber)
	status.sysdatetime = C.CString(format(&s.SystemDateTime))

	status.doors[0] = cbool(s.DoorState[1])
	status.doors[1] = cbool(s.DoorState[2])
	status.doors[2] = cbool(s.DoorState[3])
	status.doors[3] = cbool(s.DoorState[4])

	status.buttons[0] = cbool(s.DoorButton[1])
	status.buttons[1] = cbool(s.DoorButton[2])
	status.buttons[2] = cbool(s.DoorButton[3])
	status.buttons[3] = cbool(s.DoorButton[4])

	status.relays = C.uchar(s.RelayState)
	status.inputs = C.uchar(s.InputState)

	status.syserror = C.uchar(s.SystemError)
	status.seqno = C.uint(s.SequenceId)
	status.info = C.uchar(s.SpecialInfo)

	status.event.timestamp = C.CString(format(s.Event.Timestamp))
	status.event.index = C.uint(s.Event.Index)
	status.event.eventType = C.uchar(s.Event.Type)
	status.event.granted = cbool(s.Event.Granted)
	status.event.door = C.uchar(s.Event.Door)
	status.event.direction = C.uchar(s.Event.Direction)
	status.event.card = C.uint(s.Event.CardNumber)
	status.event.reason = C.uchar(s.Event.Reason)

	return nil
}
