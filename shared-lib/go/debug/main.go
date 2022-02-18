package main

/*
#include <stdbool.h>
#include <stdint.h>

typedef struct UnmanagedStruct {
    int n;
} UnmanagedStruct;

typedef struct udevice {
	uint32_t    id;
	const char *address;
	struct udevice *next;
} udevice;

typedef struct UHPPOTEN {
} UHPPOTEN;

typedef struct UHPPOTE {
	const char *bind;
	const char *broadcast;
	const char *listen;
	int         timeout;  // seconds
	udevice    *devices;  // (optional) linked list of device address
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
	"net"
	"time"
	"unsafe"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func main() {}

//export GetDevices
func GetDevices(u *C.struct_UHPPOTEN, N *C.int, list *C.uint) *C.char {
	if N == nil {
		return C.CString("invalid argument (N) - expected valid pointer")
	}

	if list == nil {
		return C.CString("invalid argument (list) - expected valid pointer to list")
	}

	_, err := makeUHPPOTE(nil)
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
