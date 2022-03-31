//go:build !debug && !tests

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

func getDevice(uu uhppote.IUHPPOTE, d *C.struct_Device, deviceID uint32) error {
	if d == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to Device struct")
	}

	device, err := uu.GetDevice(deviceID)
	if err != nil {
		return err
	}

	if device == nil {
		return fmt.Errorf("%v: no response to get-device", deviceID)
	}

	d.ID = C.uint(deviceID)
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

func getStatus(uu uhppote.IUHPPOTE, status *C.struct_Status, deviceID uint32) error {
	if status == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
	}

	s, err := uu.GetStatus(deviceID)
	if err != nil {
		return err
	} else if s == nil {
		return fmt.Errorf("%v: no response to get-status", deviceID)
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

	status.ID = C.uint(s.SerialNumber)
	status.sysdatetime = C.CString(format(&s.SystemDateTime))

	doors := unsafe.Slice(status.doors, 4)
	buttons := unsafe.Slice(status.buttons, 4)

	doors[0] = cbool(s.DoorState[1])
	doors[1] = cbool(s.DoorState[2])
	doors[2] = cbool(s.DoorState[3])
	doors[3] = cbool(s.DoorState[4])

	buttons[0] = cbool(s.DoorButton[1])
	buttons[1] = cbool(s.DoorButton[2])
	buttons[2] = cbool(s.DoorButton[3])
	buttons[3] = cbool(s.DoorButton[4])

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

func getTime(uu uhppote.IUHPPOTE, datetime **C.char, deviceID uint32) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to string")
	}

	dt, err := uu.GetTime(deviceID)
	if err != nil {
		return err
	}

	if dt == nil {
		return fmt.Errorf("%v: no response to get-time", deviceID)
	}

	*datetime = C.CString(fmt.Sprintf("%v", dt.DateTime))

	return nil
}

func setTime(uu uhppote.IUHPPOTE, deviceID uint32, datetime *C.char) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to string")
	}

	if dt, err := time.Parse("2006-01-02 15:04:05", C.GoString(datetime)); err != nil {
		return err
	} else {
		response, err := uu.SetTime(deviceID, dt)
		if err != nil {
			return err
		}

		if response == nil {
			return fmt.Errorf("%v: no response to set-time", deviceID)
		}

		return nil
	}
}

func getListener(uu uhppote.IUHPPOTE, address **C.char, deviceID uint32) error {
	if address == nil {
		return fmt.Errorf("invalid argument (address) - expected valid pointer to string")
	}

	listener, err := uu.GetListener(deviceID)
	if err != nil {
		return err
	}

	if listener == nil {
		return fmt.Errorf("%v: no response to get-listener", deviceID)
	}

	*address = C.CString(fmt.Sprintf("%v:%v", listener.Address.IP, listener.Address.Port))

	return nil
}

func setListener(uu uhppote.IUHPPOTE, deviceID uint32, listener *C.char) error {
	if listener == nil {
		return fmt.Errorf("invalid argument (listener) - expected valid pointer to string")
	}

	if address, err := net.ResolveUDPAddr("udp", C.GoString(listener)); err != nil {
		return err
	} else if address == nil || address.IP.To4() == nil {
		return fmt.Errorf("Invalid UDP address: %v", listener)
	} else {
		if response, err := uu.SetListener(deviceID, *address); err != nil {
			return err
		} else if response == nil {
			return fmt.Errorf("%v: no response to set-listener", deviceID)
		}
	}

	return nil
}
