package main

/*
#include <stdbool.h>
#include <stdint.h>

typedef struct udevice {
	uint32_t    id;
	const char *address;
} udevice;

typedef struct udevices {
	uint32_t  N;        // number of devicess
	udevice  *devices;  // array non-local devices
} udevices;

typedef struct UHPPOTE {
	const char *bind;
	const char *broadcast;
	const char *listen;
	int         timeout;  // seconds
	udevices   *devices;  // (optional) list of non-local devices
	bool        debug;
} UHPPOTE;

struct Device {
    unsigned long ID;
	char *address;
	char *subnet;
	char *gateway;
	char *MAC;
	char *version;
	char *date;
};
*/
import "C"

import (
	"fmt"
	"net"
	"time"
	"unsafe"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func main() {}

//export GetDevices
func GetDevices(u *C.struct_UHPPOTE, N *C.int, list *C.uint) *C.char {
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

//export GetDevice
func GetDevice(u *C.struct_UHPPOTE, deviceID uint32, d *C.struct_Device) *C.char {
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

func makeUHPPOTE(u *C.struct_UHPPOTE) (uhppote.IUHPPOTE, error) {
	bind := types.BindAddr{IP: []byte{0, 0, 0, 0}, Port: 0}
	broadcast := types.BroadcastAddr{IP: []byte{255, 255, 255, 255}, Port: 60000}
	listen := types.ListenAddr{IP: []byte{0, 0, 0, 0}, Port: 60001}
	timeout := 5 * time.Second
	devices := []uhppote.Device{}
	debug := false

	if u != nil {
		if s := C.GoString(u.bind); s != "" {
			if addr, err := types.ResolveBindAddr(s); err != nil {
				return nil, err
			} else if addr != nil {
				bind = *addr
			}
		}

		if s := C.GoString(u.broadcast); s != "" {
			if addr, err := types.ResolveBroadcastAddr(s); err != nil {
				return nil, err
			} else if addr != nil {
				broadcast = *addr
			}
		}

		if s := C.GoString(u.listen); s != "" {
			if addr, err := types.ResolveListenAddr(s); err != nil {
				return nil, err
			} else if addr != nil {
				listen = *addr
			}
		}

		if u.timeout > 0 {
			timeout = time.Duration(u.timeout) * time.Second
		}

		debug = bool(u.debug)

		if u.devices != nil && u.devices.N > 0 && u.devices.devices != nil {
			list := unsafe.Slice(u.devices.devices, u.devices.N)
			for _, d := range list {
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
			}
		}
	}

	fmt.Printf(">>> BIND:      %v\n", bind)
	fmt.Printf(">>> BROADCAST: %v\n", broadcast)
	fmt.Printf(">>> LISTEN:    %v\n", listen)
	fmt.Printf(">>> TIMEOUT:   %v\n", timeout)
	fmt.Printf(">>> DEBUG:     %v\n", debug)

	for _, v := range devices {
		fmt.Printf(">>> DEVICE     %-10v %v\n", v.DeviceID, v.Address)
	}

	return uhppote.NewUHPPOTE(bind, broadcast, listen, timeout, devices, debug), nil
}
