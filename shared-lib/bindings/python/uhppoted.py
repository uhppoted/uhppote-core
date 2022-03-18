import ctypes
import platform

from ctypes import c_bool
from ctypes import c_char_p
from ctypes import c_ubyte
from ctypes import c_int
from ctypes import c_longlong
from ctypes import c_uint32
from ctypes import c_ulong
from ctypes import c_void_p
from ctypes import pointer
from ctypes import byref
from ctypes import Structure
from ctypes import POINTER

from dataclasses import dataclass

if 'Windows' in platform.system():
    lib = ctypes.windll.LoadLibrary("uhppoted")
else:
    lib = ctypes.cdll.LoadLibrary("libuhppoted.so")


@dataclass
class Controller:
    id: int
    address: str


@dataclass
class UHPPOTE:
    bind: str
    broadcast: str
    listen: str
    timeout: int
    controllers: list[Controller]
    debug: bool


@dataclass
class Device:
    ID: int
    address: str
    subnet: str
    gateway: str
    MAC: str
    version: str
    date: str


@dataclass
class Event:
    timestamp: str
    index: int
    type: int
    granted: bool
    door: int
    direction: int
    card: int
    reason: int


@dataclass
class Status:
    ID: int
    sysdatetime: str
    doors: list[bool]
    buttons: list[bool]
    relays: int
    inputs: int
    syserror: int
    seqno: int
    info: int
    event: Event


class Uhppote:
    def __init__(self, uhppote=None):
        self._uhppote = None
        if uhppote:
            self._uhppote = GoUHPPOTE(uhppote.bind, uhppote.broadcast,
                                      uhppote.listen, uhppote.timeout,
                                      uhppote.controllers, uhppote.debug)

    @staticmethod
    def errcheck(err, func, args):
        if err:
            raise Exception(f"{err.decode('utf-8')}")
        else:
            return args

    def get_devices(self):
        GetDevices = lib.GetDevices
        GetDevices.argtypes = [
            POINTER(GoUHPPOTE),
            POINTER(ctypes.c_int),
            POINTER(ctypes.c_uint32)
        ]
        GetDevices.restype = ctypes.c_char_p
        GetDevices.errcheck = self.errcheck

        N = 0
        while True:
            N = N + 16
            count = ctypes.c_int(N)
            list = (c_uint32 * N)(*[0] * N)

            GetDevices(self._uhppote, byref(count), list)

            if count.value <= N:
                break

        return list[0:count.value]

    def get_device(self, deviceID):
        GetDevice = lib.GetDevice
        GetDevice.argtypes = [POINTER(GoUHPPOTE), POINTER(GoDevice), c_ulong]
        GetDevice.restype = ctypes.c_char_p
        GetDevice.errcheck = self.errcheck

        device = GoDevice()

        GetDevice(self._uhppote, byref(device), deviceID)

        return Device(device.ID, device.address.decode('utf-8'),
                      device.subnet.decode('utf-8'),
                      device.gateway.decode('utf-8'),
                      device.MAC.decode('utf-8'),
                      device.version.decode('utf-8'),
                      device.date.decode('utf-8'))

    def set_address(self, deviceID, address, subnet, gateway):
        SetAddress = lib.SetAddress
        SetAddress.argtypes = [
            POINTER(GoUHPPOTE), c_ulong, c_char_p, c_char_p, c_char_p
        ]
        SetAddress.restype = ctypes.c_char_p
        SetAddress.errcheck = self.errcheck

        SetAddress(self._uhppote, deviceID, c_char_p(bytes(address, 'utf-8')),
                   c_char_p(bytes(subnet, 'utf-8')),
                   c_char_p(bytes(gateway, 'utf-8')))

    def get_status(self, deviceID):
        GetStatus = lib.GetStatus
        GetStatus.argtypes = [POINTER(GoUHPPOTE), POINTER(GoStatus), c_ulong]
        GetStatus.restype = ctypes.c_char_p
        GetStatus.errcheck = self.errcheck

        status = GoStatus()

        status.doors = (c_ubyte * 4)(*[0] * 4)
        status.buttons = (c_ubyte * 4)(*[0] * 4)
        status.event = pointer(GoEvent())

        GetStatus(self._uhppote, ctypes.byref(status), deviceID)

        doors = [False, False, False, False]
        buttons = [False, False, False, False]
        for i in range(4):
            if status.doors[i] != 0:
                doors[i] = True

            if status.buttons[i] != 0:
                buttons[i] = True

        event = Event(
            status.event.contents.timestamp.decode('utf-8'),
            status.event.contents.index,
            status.event.contents.type,
            status.event.contents.granted,
            status.event.contents.door,
            status.event.contents.direction,
            status.event.contents.card,
            status.event.contents.reason,
        )

        return Status(status.ID, status.sysdatetime.decode('utf-8'), doors,
                      buttons, status.relays, status.inputs, status.syserror,
                      status.seqno, status.info, event)


# INTERNAL TYPES
class GoController(Structure):
    _fields_ = [('id', c_uint32), ('address', c_char_p)]


class GoControllers(Structure):
    _fields_ = [('N', c_uint32), ('devices', POINTER(GoController))]


class GoUHPPOTE(Structure):
    _fields_ = [('bind', c_char_p), ('broadcast', c_char_p),
                ('listen', c_char_p), ('timeout', c_int), ('ndevices', c_int),
                ('devices', POINTER(GoControllers)), ('debug', c_bool)]

    def __init__(self, bind, broadcast, listen, timeout, controllers, debug):
        super(GoUHPPOTE, self).__init__()
        self.bind = c_char_p(bytes(bind, 'utf-8'))
        self.broadcast = c_char_p(bytes(broadcast, 'utf-8'))
        self.listen = c_char_p(bytes(listen, 'utf-8'))
        self.timeout = timeout
        self.devices = None
        self.debug = c_bool(debug)

        N = len(controllers)
        if N > 0:
            list = GoControllers(N, (GoController * N)())

            for ix, c in enumerate(controllers):
                list.devices[ix] = GoController(
                    c.id, c_char_p(bytes(c.address, 'utf-8')))

            self.devices = pointer(list)


class GoDevice(Structure):
    _fields_ = [
        ('ID', c_ulong),
        ('address', c_char_p),
        ('subnet', c_char_p),
        ('gateway', c_char_p),
        ('MAC', c_char_p),
        ('version', c_char_p),
        ('date', c_char_p),
    ]


class GoEvent(Structure):
    _fields_ = [
        ('timestamp', c_char_p),
        ('index', c_uint32),
        ('type', c_ubyte),
        ('granted', c_bool),
        ('door', c_ubyte),
        ('direction', c_ubyte),
        ('card', c_uint32),
        ('reason', c_ubyte),
    ]


class GoStatus(Structure):
    _fields_ = [
        ('ID', c_uint32),
        ('sysdatetime', c_char_p),
        ('doors', POINTER(c_ubyte)),  #     uint8_t[4]
        ('buttons', POINTER(c_ubyte)),  #     uint8_t[4]
        ('relays', c_ubyte),
        ('inputs', c_ubyte),
        ('syserror', c_ubyte),
        ('info', c_ubyte),
        ('seqno', c_uint32),
        ('event', POINTER(GoEvent)),
    ]