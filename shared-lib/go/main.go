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
	int         timeout;  // milliseconds
	udevices   *devices;  // (optional) list of non-local devices
	bool        debug;
} UHPPOTE;

typedef struct Device {
    uint32_t ID;
	char *address;
	char *subnet;
	char *gateway;
	char *MAC;
	char *version;
	char *date;
} Device;

typedef struct Event {
	char  *timestamp;
    uint32_t index;
	uint8_t eventType;
	uint8_t granted;
	uint8_t door;
	uint8_t direction;
	uint32_t card;
	uint8_t reason;
} Event;

typedef struct Status {
    uint32_t ID;
	char *sysdatetime;
	uint8_t  *doors;   // uint_8[4]
	uint8_t  *buttons; // uint_8[4]
	uint8_t relays;
	uint8_t inputs;
	uint8_t syserror;
	uint8_t info;
	uint32_t seqno;
	Event *event;
} Status;

typedef struct DoorControl {
    uint8_t mode;
    uint8_t delay;
} DoorControl;

*/
import "C"

import (
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

//export GetTime
func GetTime(u *C.struct_UHPPOTE, datetime **C.char, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getTime(uu, datetime, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetTime
func SetTime(u *C.struct_UHPPOTE, deviceID uint32, datetime *C.char) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setTime(uu, deviceID, datetime); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetListener
func GetListener(u *C.struct_UHPPOTE, address **C.char, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getListener(uu, address, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetListener
func SetListener(u *C.struct_UHPPOTE, deviceID uint32, listener *C.char) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setListener(uu, deviceID, listener); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetDoorControl
func GetDoorControl(u *C.struct_UHPPOTE, control *C.struct_DoorControl, deviceID uint32, door uint8) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getDoorControl(uu, control, deviceID, door); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetDoorControl
func SetDoorControl(u *C.struct_UHPPOTE, deviceID uint32, door uint8, mode uint8, delay uint8) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setDoorControl(uu, deviceID, door, types.ControlState(mode), delay); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetCards
func GetCards(u *C.struct_UHPPOTE, N *C.int, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getCards(uu, N, deviceID); err != nil {
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
			timeout = time.Duration(u.timeout) * time.Millisecond
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

	return uhppote.NewUHPPOTE(bind, broadcast, listen, timeout, devices, debug), nil
}
