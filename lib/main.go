package main

/*
struct Devices {
    int X;
    int Y;
    int Z[1];
};

struct Device {
    unsigned long ID;
	char *        address;
	char *        subnet;
	char *        gateway;
	char *        MAC;
	char *        version;
	char *        date;
};
*/
import "C"

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

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
func GetDevice(deviceID uint32) (C.struct_Device, *C.char) {
	bind, err := types.ResolveBindAddr("192.168.1.100")
	if err != nil {
		return C.struct_Device{}, C.CString(err.Error())
	}

	broadcast, err := types.ResolveBroadcastAddr("192.168.1.255")
	if err != nil {
		return C.struct_Device{}, C.CString(err.Error())
	}

	listen, err := types.ResolveListenAddr("192.168.1.100:60001")
	if err != nil {
		return C.struct_Device{}, C.CString(err.Error())
	}

	timeout := 5 * time.Second
	devices := []uhppote.Device{}

	u := uhppote.NewUHPPOTE(*bind, *broadcast, *listen, timeout, devices, true)

	device, err := u.GetDevice(deviceID)
	if err != nil {
		return C.struct_Device{}, C.CString(err.Error())
	}

	return C.struct_Device{
		ID:      C.ulong(device.SerialNumber),
		address: C.CString(fmt.Sprintf("%v", device.IpAddress)),
		subnet:  C.CString(fmt.Sprintf("%v", device.SubnetMask)),
		gateway: C.CString(fmt.Sprintf("%v", device.Gateway)),
		MAC:     C.CString(fmt.Sprintf("%v", device.MacAddress)),
		version: C.CString(fmt.Sprintf("%v", device.Version)),
		date:    C.CString(fmt.Sprintf("%v", device.Date)),
	}, nil
}

func main() {}
