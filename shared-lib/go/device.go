//go:build !debug && !tests

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

	response, err := uu.GetDevice(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-device", deviceID)
	}

	d.ID = C.uint(deviceID)
	d.address = C.CString(fmt.Sprintf("%v", response.IpAddress))
	d.subnet = C.CString(fmt.Sprintf("%v", response.SubnetMask))
	d.gateway = C.CString(fmt.Sprintf("%v", response.Gateway))
	d.MAC = C.CString(fmt.Sprintf("%v", response.MacAddress))
	d.version = C.CString(fmt.Sprintf("%v", response.Version))
	d.date = C.CString(fmt.Sprintf("%v", response.Date))

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

	if response, err := uu.SetAddress(deviceID, _address, _subnet, _gateway); err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("invalid reply from device (%v)", response)
	} else if !response.Succeeded {
		return fmt.Errorf("failed to set device address")
	}

	return nil
}

func getStatus(uu uhppote.IUHPPOTE, status *C.struct_Status, deviceID uint32) error {
	if status == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
	}

	response, err := uu.GetStatus(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-status", deviceID)
	}

	format := func(t *types.DateTime) string {
		if t != nil {
			return time.Time(*t).Format("2006-01-02 15:04:05")
		}

		return ""
	}

	status.ID = C.uint(response.SerialNumber)
	status.sysdatetime = C.CString(format(&response.SystemDateTime))

	doors := unsafe.Slice(status.doors, 4)
	buttons := unsafe.Slice(status.buttons, 4)

	doors[0] = cbool(response.DoorState[1])
	doors[1] = cbool(response.DoorState[2])
	doors[2] = cbool(response.DoorState[3])
	doors[3] = cbool(response.DoorState[4])

	buttons[0] = cbool(response.DoorButton[1])
	buttons[1] = cbool(response.DoorButton[2])
	buttons[2] = cbool(response.DoorButton[3])
	buttons[3] = cbool(response.DoorButton[4])

	status.relays = C.uchar(response.RelayState)
	status.inputs = C.uchar(response.InputState)

	status.syserror = C.uchar(response.SystemError)
	status.seqno = C.uint(response.SequenceId)
	status.info = C.uchar(response.SpecialInfo)

	status.event.timestamp = C.CString(format(response.Event.Timestamp))
	status.event.index = C.uint(response.Event.Index)
	status.event.eventType = C.uchar(response.Event.Type)
	status.event.granted = cbool(response.Event.Granted)
	status.event.door = C.uchar(response.Event.Door)
	status.event.direction = C.uchar(response.Event.Direction)
	status.event.card = C.uint(response.Event.CardNumber)
	status.event.reason = C.uchar(response.Event.Reason)

	return nil
}

func getTime(uu uhppote.IUHPPOTE, datetime **C.char, deviceID uint32) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to string")
	}

	response, err := uu.GetTime(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-time", deviceID)
	}

	*datetime = C.CString(fmt.Sprintf("%v", response.DateTime))

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
		} else if response == nil {
			return fmt.Errorf("%v: no response to set-time", deviceID)
		}

		return nil
	}
}

func getListener(uu uhppote.IUHPPOTE, listener **C.char, deviceID uint32) error {
	if listener == nil {
		return fmt.Errorf("invalid argument (address) - expected valid pointer to string")
	}

	response, err := uu.GetListener(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-listener", deviceID)
	}

	*listener = C.CString(fmt.Sprintf("%v:%v", response.Address.IP, response.Address.Port))

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

func getDoorControl(uu uhppote.IUHPPOTE, control *C.struct_DoorControl, deviceID uint32, door uint8) error {
	if control == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to DoorControl struct")
	}

	response, err := uu.GetDoorControlState(deviceID, door)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-door-control-state", deviceID)
	}

	control.mode = C.uchar(response.ControlState)
	control.delay = C.uchar(response.Delay)

	return nil
}

func setDoorControl(uu uhppote.IUHPPOTE, deviceID uint32, door uint8, mode types.ControlState, delay uint8) error {
	response, err := uu.SetDoorControlState(deviceID, door, mode, delay)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to set-door-control-state", deviceID)
	}

	return nil
}

func openDoor(uu uhppote.IUHPPOTE, deviceID uint32, door uint8) error {
	response, err := uu.OpenDoor(deviceID, door)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to open door %d", deviceID, door)
	}

	return nil
}
