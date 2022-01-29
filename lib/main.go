package main

/*
struct Devices {
    int X;
    int Y;
    int Z[1];
};

struct Device {
    unsigned long ID;
};
*/
import "C"

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

//export InterOp
func InterOp(path *C.char) (int, *C.char) {
	value := 98765
	err := fmt.Errorf("OOOPS")

	return value, C.CString(err.Error())
}

//export GetDevices
func GetDevices(list []C.ulong) (C.int, *C.char) {
	bind, err := types.ResolveBindAddr("192.168.1.100")
	if err != nil {
		return 0, C.CString(err.Error())
	}

	broadcast, err := types.ResolveBroadcastAddr("192.168.1.255")
	if err != nil {
		return 0, C.CString(err.Error())
	}

	listen, err := types.ResolveListenAddr("192.168.1.100:60001")
	if err != nil {
		return 0, C.CString(err.Error())
	}

	timeout := 5 * time.Second
	devices := []uhppote.Device{}

	u := uhppote.NewUHPPOTE(*bind, *broadcast, *listen, timeout, devices, true)

	if devices, err := u.GetDevices(); err != nil {
		return 0, C.CString(err.Error())
	} else {
		for ix, device := range devices {
			if ix < len(list) {
				list[ix] = C.ulong(device.SerialNumber)
			} else {
				break
			}
		}

		return C.int(len(devices)), nil
	}
}

//export GetDevice
func GetDevice(deviceID uint32) C.struct_Device {
	bind, err := types.ResolveBindAddr("192.168.1.100")
	if err != nil {
		return C.struct_Device{0}
	}

	broadcast, err := types.ResolveBroadcastAddr("192.168.1.255")
	if err != nil {
		return C.struct_Device{0}
	}

	listen, err := types.ResolveListenAddr("192.168.1.100:60001")
	if err != nil {
		return C.struct_Device{0}
	}

	timeout := 1 * time.Second
	devices := []uhppote.Device{}

	u := uhppote.NewUHPPOTE(*bind, *broadcast, *listen, timeout, devices, true)

	if device, err := u.GetDevice(deviceID); err != nil {
		return C.struct_Device{0}
	} else {
		return C.struct_Device{C.ulong(device.SerialNumber)}
	}
}

func main() {}
