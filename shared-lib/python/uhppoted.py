import ctypes
import platform

from ctypes import c_bool
from ctypes import c_char_p
from ctypes import c_int
from ctypes import c_longlong
from ctypes import c_uint32
from ctypes import c_ulong
from ctypes import pointer
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

            GetDevices(self._uhppote, ctypes.byref(count), list)

            if count.value <= N:
                break

        return list[0:count.value]

    def get_device(self, deviceID):
        GetDevice = lib.GetDevice
        GetDevice.argtypes = [POINTER(GoUHPPOTE), c_ulong, POINTER(GoDevice)]
        GetDevice.restype = ctypes.c_char_p
        GetDevice.errcheck = self.errcheck

        device = GoDevice()

        GetDevice(self._uhppote, deviceID, ctypes.byref(device))

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
