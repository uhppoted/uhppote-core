package main

/*

typedef struct udevice {
	unsigned        id;
	const char     *address;
	struct udevice *next;
} udevice;

typedef struct UHPPOTE {
	const char *bind;
	const char *broadcast;
	const char *listen;
	int         timeout;  // seconds
	udevice    *devices;  // (optional) linked list of device address
	int         debug;    // true/false
} UHPPOTE;

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
	"net"
	"time"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func main() {}

//export GetDevices
func GetDevices(u *C.struct_UHPPOTE, list []C.ulong) (C.int, *C.char) {
	uu, err := makeUHPPOTE(u)
	if err != nil {
		return 0, C.CString(err.Error())
	}

	if devices, err := uu.GetDevices(); err != nil {
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
func GetDevice(u *C.struct_UHPPOTE, deviceID uint32) (C.struct_Device, *C.char) {
	uu, err := makeUHPPOTE(u)
	if err != nil {
		return C.struct_Device{}, C.CString(err.Error())
	}

	device, err := uu.GetDevice(deviceID)
	if err != nil {
		return C.struct_Device{}, C.CString(err.Error())
	}

	if device == nil {
		return C.struct_Device{}, C.CString(fmt.Errorf("No device found for %v", deviceID).Error())
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

func makeUHPPOTE(u *C.struct_UHPPOTE) (uhppote.IUHPPOTE, error) {
	bind := types.BindAddr{IP: []byte{0, 0, 0, 0}, Port: 0}
	broadcast := types.BroadcastAddr{IP: []byte{255, 255, 255, 255}, Port: 60000}
	listen := types.ListenAddr{IP: []byte{0, 0, 0, 0}, Port: 60001}
	timeout := 5 * time.Second
	devices := []uhppote.Device{}
	debug := false

	if u != nil {
		if addr, err := types.ResolveBindAddr(C.GoString(u.bind)); err != nil {
			return nil, err
		} else if addr != nil {
			bind = *addr
		}

		if addr, err := types.ResolveBroadcastAddr(C.GoString(u.broadcast)); err != nil {
			return nil, err
		} else if addr != nil {
			broadcast = *addr
		}

		if addr, err := types.ResolveListenAddr(C.GoString(u.listen)); err != nil {
			return nil, err
		} else if addr != nil {
			listen = *addr
		}

		timeout = time.Duration(u.timeout) * time.Second
		debug = u.debug != 0

		d := u.devices
		for d != nil {
			if d.id != 0 {
				addr, err := types.ResolveAddr(C.GoString(d.address))
				if err != nil {
					return nil, err
				}

				devices = append(devices, uhppote.Device{
					DeviceID: uint32(d.id),
					Address:  (*net.UDPAddr)(addr),
				})
			}

			d = d.next
		}
	}

	return uhppote.NewUHPPOTE(bind, broadcast, listen, timeout, devices, debug), nil

}
