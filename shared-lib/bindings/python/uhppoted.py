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
from typing import Final

if 'Windows' in platform.system():
    lib = ctypes.windll.LoadLibrary("uhppoted")
else:
    lib = ctypes.cdll.LoadLibrary("libuhppoted.so")

NORMALLY_OPEN: Final[int] = 1
NORMALLY_CLOSED: Final[int] = 2
CONTROLLED: Final[int] = 3


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


@dataclass
class DoorControl:
    mode: int
    delay: int


@dataclass
class Card:
    cardNumber: int
    start: str
    end: str
    doors: list[int]


class Uhppote:
    def __init__(self, uhppote=None):
        self._uhppote = None
        if uhppote:
            self._uhppote = GoUHPPOTE(uhppote.bind, uhppote.broadcast, uhppote.listen,
                                      uhppote.timeout, uhppote.controllers, uhppote.debug)

    @staticmethod
    def errcheck(err, func, args):
        if err:
            raise Exception(f"{err.decode('utf-8')}")
        else:
            return args

    def get_devices(self):
        GetDevices = lib.GetDevices
        GetDevices.argtypes = [POINTER(GoUHPPOTE), POINTER(ctypes.c_int), POINTER(ctypes.c_uint32)]
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

        return Device(device.ID, device.address.decode('utf-8'), device.subnet.decode('utf-8'),
                      device.gateway.decode('utf-8'), device.MAC.decode('utf-8'),
                      device.version.decode('utf-8'), device.date.decode('utf-8'))

    def set_address(self, deviceID, address, subnet, gateway):
        SetAddress = lib.SetAddress
        SetAddress.argtypes = [POINTER(GoUHPPOTE), c_ulong, c_char_p, c_char_p, c_char_p]
        SetAddress.restype = ctypes.c_char_p
        SetAddress.errcheck = self.errcheck

        SetAddress(self._uhppote, deviceID, c_char_p(bytes(address, 'utf-8')),
                   c_char_p(bytes(subnet, 'utf-8')), c_char_p(bytes(gateway, 'utf-8')))

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

        return Status(status.ID, status.sysdatetime.decode('utf-8'), doors, buttons, status.relays,
                      status.inputs, status.syserror, status.seqno, status.info, event)

    def get_time(self, deviceID):
        GetTime = lib.GetTime
        GetTime.argtypes = [POINTER(GoUHPPOTE), POINTER(c_char_p), c_ulong]
        GetTime.restype = ctypes.c_char_p
        GetTime.errcheck = self.errcheck

        datetime = c_char_p()

        GetTime(self._uhppote, byref(datetime), deviceID)

        return datetime.value.decode('utf-8')

    def set_time(self, deviceID, datetime):
        SetTime = lib.SetTime
        SetTime.argtypes = [POINTER(GoUHPPOTE), c_ulong, c_char_p]
        SetTime.restype = ctypes.c_char_p
        SetTime.errcheck = self.errcheck

        SetTime(self._uhppote, deviceID, c_char_p(bytes(datetime, 'utf-8')))

    def get_listener(self, deviceID):
        GetListener = lib.GetListener
        GetListener.argtypes = [POINTER(GoUHPPOTE), POINTER(c_char_p), c_ulong]
        GetListener.restype = ctypes.c_char_p
        GetListener.errcheck = self.errcheck

        listener = c_char_p()

        GetListener(self._uhppote, byref(listener), deviceID)

        return listener.value.decode('utf-8')

    def set_listener(self, deviceID, listener):
        SetListener = lib.SetListener
        SetListener.argtypes = [POINTER(GoUHPPOTE), c_ulong, c_char_p]
        SetListener.restype = ctypes.c_char_p
        SetListener.errcheck = self.errcheck

        SetListener(self._uhppote, deviceID, c_char_p(bytes(listener, 'utf-8')))

    def get_door_control(self, deviceID, door):
        GetDoorControl = lib.GetDoorControl
        GetDoorControl.argtypes = [POINTER(GoUHPPOTE), POINTER(GoDoorControl), c_ulong, c_ubyte]
        GetDoorControl.restype = ctypes.c_char_p
        GetDoorControl.errcheck = self.errcheck

        control = GoDoorControl()

        GetDoorControl(self._uhppote, byref(control), deviceID, door)

        return DoorControl(control.control, control.delay)

    def set_door_control(self, deviceID, door, mode, delay):
        SetDoorControl = lib.SetDoorControl
        SetDoorControl.argtypes = [POINTER(GoUHPPOTE), c_ulong, c_ubyte, c_ubyte, c_ubyte]
        SetDoorControl.restype = ctypes.c_char_p
        SetDoorControl.errcheck = self.errcheck

        SetDoorControl(self._uhppote, deviceID, door, mode, delay)

    def get_cards(self, deviceID):
        GetCards = lib.GetCards
        GetCards.argtypes = [POINTER(GoUHPPOTE), POINTER(c_int), c_ulong]
        GetCards.restype = ctypes.c_char_p
        GetCards.errcheck = self.errcheck

        cards = ctypes.c_int(0)

        GetCards(self._uhppote, byref(cards), deviceID)

        return cards.value

    def get_card(self, deviceID, cardNumber):
        GetCard = lib.GetCard
        GetCard.argtypes = [POINTER(GoUHPPOTE), POINTER(GoCard), c_ulong, c_ulong]
        GetCard.restype = ctypes.c_char_p
        GetCard.errcheck = self.errcheck

        card = GoCard()

        card.doors = (c_ubyte * 4)(*[0] * 4)

        GetCard(self._uhppote, byref(card), deviceID, cardNumber)

        doors = [0, 0, 0, 0]

        for i in range(4):
            doors[i] = card.doors[i]

        return Card(card.cardNumber, card.start.decode('utf-8'), card.end.decode('utf-8'), doors)

    def get_card_by_index(self, deviceID, index):
        GetCardByIndex = lib.GetCardByIndex
        GetCardByIndex.argtypes = [POINTER(GoUHPPOTE), POINTER(GoCard), c_ulong, c_ulong]
        GetCardByIndex.restype = ctypes.c_char_p
        GetCardByIndex.errcheck = self.errcheck

        card = GoCard()

        card.doors = (c_ubyte * 4)(*[0] * 4)

        GetCardByIndex(self._uhppote, byref(card), deviceID, index)

        doors = [0, 0, 0, 0]

        for i in range(4):
            doors[i] = card.doors[i]

        return Card(card.cardNumber, card.start.decode('utf-8'), card.end.decode('utf-8'), doors)

    def put_card(self, deviceID, cardNumber, start, end, doors):
        PutCard = lib.PutCard
        PutCard.argtypes = [
            POINTER(GoUHPPOTE), c_ulong, c_ulong, c_char_p, c_char_p,
            POINTER(c_ubyte)
        ]
        PutCard.restype = ctypes.c_char_p
        PutCard.errcheck = self.errcheck

        _doors = (c_ubyte * 4)(*[0] * 4)
        _doors[0] = doors[0]
        _doors[1] = doors[1]
        _doors[2] = doors[2]
        _doors[3] = doors[3]

        PutCard(self._uhppote, deviceID, cardNumber, c_char_p(bytes(start, 'utf-8')),
                c_char_p(bytes(end, 'utf-8')), _doors)

    def delete_card(self, deviceID, cardNumber):
        DeleteCard = lib.DeleteCard
        DeleteCard.argtypes = [POINTER(GoUHPPOTE), c_ulong, c_ulong]
        DeleteCard.restype = ctypes.c_char_p
        DeleteCard.errcheck = self.errcheck

        DeleteCard(self._uhppote, deviceID, cardNumber)

    def delete_cards(self, deviceID):
        DeleteCards = lib.DeleteCards
        DeleteCards.argtypes = [POINTER(GoUHPPOTE), c_ulong]
        DeleteCards.restype = ctypes.c_char_p
        DeleteCards.errcheck = self.errcheck

        DeleteCards(self._uhppote, deviceID)


# INTERNAL TYPES
class GoController(Structure):
    _fields_ = [('id', c_uint32), ('address', c_char_p)]


class GoControllers(Structure):
    _fields_ = [('N', c_uint32), ('devices', POINTER(GoController))]


class GoUHPPOTE(Structure):
    _fields_ = [('bind', c_char_p), ('broadcast', c_char_p), ('listen', c_char_p),
                ('timeout', c_int), ('ndevices', c_int), ('devices', POINTER(GoControllers)),
                ('debug', c_bool)]

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
                list.devices[ix] = GoController(c.id, c_char_p(bytes(c.address, 'utf-8')))

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
        ('doors', POINTER(c_ubyte)),  # uint8_t[4]
        ('buttons', POINTER(c_ubyte)),  # uint8_t[4]
        ('relays', c_ubyte),
        ('inputs', c_ubyte),
        ('syserror', c_ubyte),
        ('info', c_ubyte),
        ('seqno', c_uint32),
        ('event', POINTER(GoEvent)),
    ]


class GoDoorControl(Structure):
    _fields_ = [
        ('control', c_ubyte),
        ('delay', c_ubyte),
    ]


class GoCard(Structure):
    _fields_ = [
        ('cardNumber', c_uint32),
        ('start', c_char_p),
        ('end', c_char_p),
        ('doors', POINTER(c_ubyte)),  # uint8_t[4]
    ]
