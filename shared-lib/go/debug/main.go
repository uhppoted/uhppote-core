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

typedef struct Device {
    unsigned long ID;
	char *address;
	char *subnet;
	char *gateway;
	char *MAC;
	char *version;
	char *date;
} Device;

typedef struct Event {
	char  *timestamp;
    unsigned long index;
	uint8_t eventType;
	uint8_t granted;
	uint8_t door;
	uint8_t direction;
	unsigned long card;
	uint8_t reason;
} Event;

typedef struct Status {
    unsigned long ID;
	char *sysdatetime;
	uint8_t  *doors;   // uint_8[4]
	uint8_t  *buttons; // uint_8[4]
	uint8_t relays;
	uint8_t inputs;
	uint8_t syserror;
	uint8_t info;
	unsigned long seqno;   // Note: uint32_t causes CGO to pack the struct on a 4 byte boundary - CCL always packs on 8 byte boundaries
	Event *event;
} Status;

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

var DEBUG bool

func main() {}

//export GetDevices
func GetDevices(u *C.struct_UHPPOTE, N *C.int, list *C.uint) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getDevices(uu, N, list); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetDevice
func GetDevice(u *C.struct_UHPPOTE, device *C.struct_Device, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getDevice(uu, device, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetAddress
func SetAddress(u *C.struct_UHPPOTE, deviceID uint32, addr, subnet, gateway *C.char) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setAddress(uu, deviceID, addr, subnet, gateway); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetStatus
func GetStatus(u *C.struct_UHPPOTE, status *C.struct_Status, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getStatus(uu, status, deviceID); err != nil {
		return C.CString(err.Error())
	}

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

	DEBUG = debug

	if DEBUG {
		fmt.Printf(">>> UHPPOTE\n")
		fmt.Printf("    bind:      %v\n", bind)
		fmt.Printf("    broadcast: %v\n", broadcast)
		fmt.Printf("    listen:    %v\n", listen)
		fmt.Printf("    timeout:   %v\n", timeout)
		fmt.Printf("    debug:     %v\n", debug)

		for _, v := range devices {
			fmt.Printf("    device     %-10v %v\n", v.DeviceID, v.Address)
		}

		fmt.Println()
	}

	return uhppote.NewUHPPOTE(bind, broadcast, listen, timeout, devices, debug), nil
}
