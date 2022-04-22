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
from functools import cache
from types import SimpleNamespace
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
    eventType: int
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


@dataclass
class TimeProfile:
    ID: int
    linked: int
    start: str
    end: str
    monday: bool
    tuesday: bool
    wednesday: bool
    thursday: bool
    friday: bool
    saturday: bool
    sunday: bool
    segment1start: str
    segment1end: str
    segment2start: str
    segment2end: str
    segment3start: str
    segment3end: str


class Uhppote:
    def __init__(self, uhppote=None):
        self.ffi = FFI(self.errcheck)
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
        N = 0
        while True:
            N = N + 16
            count = ctypes.c_int(N)
            list = (c_uint32 * N)(*[0] * N)

            self.ffi.GetDevices(self._uhppote, byref(count), list)

            if count.value <= N:
                break

        return list[0:count.value]

    def get_device(self, deviceID):
        device = GoDevice()

        self.ffi.GetDevice(self._uhppote, byref(device), deviceID)

        return Device(device.ID, device.address.decode('utf-8'), device.subnet.decode('utf-8'),
                      device.gateway.decode('utf-8'), device.MAC.decode('utf-8'),
                      device.version.decode('utf-8'), device.date.decode('utf-8'))

    def set_address(self, deviceID, address, subnet, gateway):
        self.ffi.SetAddress(self._uhppote, deviceID, c_char_p(bytes(address, 'utf-8')),
                            c_char_p(bytes(subnet, 'utf-8')), c_char_p(bytes(gateway, 'utf-8')))

    def get_status(self, deviceID):
        status = GoStatus()
        status.doors = (c_ubyte * 4)(*[0] * 4)
        status.buttons = (c_ubyte * 4)(*[0] * 4)
        status.event = pointer(GoEvent())

        self.ffi.GetStatus(self._uhppote, ctypes.byref(status), deviceID)

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
            status.event.contents.eventType,
            status.event.contents.granted,
            status.event.contents.door,
            status.event.contents.direction,
            status.event.contents.card,
            status.event.contents.reason,
        )

        return Status(status.ID, status.sysdatetime.decode('utf-8'), doors, buttons, status.relays,
                      status.inputs, status.syserror, status.seqno, status.info, event)

    def get_time(self, deviceID):
        datetime = c_char_p()

        self.ffi.GetTime(self._uhppote, byref(datetime), deviceID)

        return datetime.value.decode('utf-8')

    def set_time(self, deviceID, datetime):
        self.ffi.SetTime(self._uhppote, deviceID, c_char_p(bytes(datetime, 'utf-8')))

    def get_listener(self, deviceID):
        listener = c_char_p()

        self.ffi.GetListener(self._uhppote, byref(listener), deviceID)

        return listener.value.decode('utf-8')

    def set_listener(self, deviceID, listener):
        self.ffi.SetListener(self._uhppote, deviceID, c_char_p(bytes(listener, 'utf-8')))

    def get_door_control(self, deviceID, door):
        control = GoDoorControl()

        self.ffi.GetDoorControl(self._uhppote, byref(control), deviceID, door)

        return DoorControl(control.control, control.delay)

    def set_door_control(self, deviceID, door, mode, delay):
        self.ffi.SetDoorControl(self._uhppote, deviceID, door, mode, delay)

    def open_door(self, deviceID, door):
        self.ffi.OpenDoor(self._uhppote, deviceID, door)

    def get_cards(self, deviceID):
        cards = ctypes.c_int(0)

        self.ffi.GetCards(self._uhppote, byref(cards), deviceID)

        return cards.value

    def get_card(self, deviceID, cardNumber):
        card = GoCard()
        card.doors = (c_ubyte * 4)(*[0] * 4)

        self.ffi.GetCard(self._uhppote, byref(card), deviceID, cardNumber)

        doors = [0, 0, 0, 0]
        for i in range(4):
            doors[i] = card.doors[i]

        return Card(card.cardNumber, card.start.decode('utf-8'), card.end.decode('utf-8'), doors)

    def get_card_by_index(self, deviceID, index):
        card = GoCard()
        card.doors = (c_ubyte * 4)(*[0] * 4)

        self.ffi.GetCardByIndex(self._uhppote, byref(card), deviceID, index)

        doors = [0, 0, 0, 0]
        for i in range(4):
            doors[i] = card.doors[i]

        return Card(card.cardNumber, card.start.decode('utf-8'), card.end.decode('utf-8'), doors)

    def put_card(self, deviceID, cardNumber, start, end, doors):
        _doors = (c_ubyte * 4)(*[0] * 4)
        _doors[0] = doors[0]
        _doors[1] = doors[1]
        _doors[2] = doors[2]
        _doors[3] = doors[3]

        self.ffi.PutCard(self._uhppote, deviceID, cardNumber, c_char_p(bytes(start, 'utf-8')),
                         c_char_p(bytes(end, 'utf-8')), _doors)

    def delete_card(self, deviceID, cardNumber):
        self.ffi.DeleteCard(self._uhppote, deviceID, cardNumber)

    def delete_cards(self, deviceID):
        self.ffi.DeleteCards(self._uhppote, deviceID)

    def get_event_index(self, deviceID):
        index = ctypes.c_ulong(0)

        self.ffi.GetEventIndex(self._uhppote, byref(index), deviceID)

        return index.value

    def set_event_index(self, deviceID, index):
        self.ffi.SetEventIndex(self._uhppote, deviceID, index)

    def get_event(self, deviceID, index):
        event = GoEvent()

        self.ffi.GetEvent(self._uhppote, byref(event), deviceID, index)

        return Event(event.timestamp.decode('utf-8'), event.index, event.eventType, event.granted,
                     event.door, event.direction, event.card, event.reason)

    def record_special_events(self, deviceID, enabled):
        self.ffi.RecordSpecialEvents(self._uhppote, deviceID, enabled)

    def get_time_profile(self, deviceID, profileID):
        profile = GoTimeProfile()

        self.ffi.GetTimeProfile(self._uhppote, byref(profile), deviceID, profileID)

        return TimeProfile(
            profile.ID, profile.linked, profile.start.decode('utf-8'), profile.end.decode('utf-8'),
            profile.monday != 0, profile.tuesday != 0, profile.wednesday != 0,
            profile.thursday != 0, profile.friday != 0, profile.saturday != 0, profile.sunday != 0,
            profile.segment1start.decode('utf-8'), profile.segment1end.decode('utf-8'),
            profile.segment2start.decode('utf-8'), profile.segment2end.decode('utf-8'),
            profile.segment3start.decode('utf-8'), profile.segment3end.decode('utf-8'))

    def set_time_profile(self, deviceID, p):
        profile = GoTimeProfile(p.ID, p.linked, c_char_p(bytes(p.start, 'utf-8')),
                                c_char_p(bytes(p.end, 'utf-8')), 1 if p.monday else 0,
                                1 if p.tuesday else 0, 1 if p.wednesday else 0,
                                1 if p.thursday else 0, 1 if p.friday else 0,
                                1 if p.saturday else 0, 1 if p.sunday else 0,
                                c_char_p(bytes(p.segment1start, 'utf-8')),
                                c_char_p(bytes(p.segment1end, 'utf-8')),
                                c_char_p(bytes(p.segment2start, 'utf-8')),
                                c_char_p(bytes(p.segment2end, 'utf-8')),
                                c_char_p(bytes(p.segment3start, 'utf-8')),
                                c_char_p(bytes(p.segment3end, 'utf-8')))

        self.ffi.SetTimeProfile(self._uhppote, deviceID, byref(profile))


# Go FFI types


class FFI:
    def __init__(self, errcheck):
        self.GetDevices = ffi('GetDevices', errcheck)
        self.GetDevice = ffi('GetDevice', errcheck)
        self.SetAddress = ffi('SetAddress', errcheck)
        self.GetStatus = ffi('GetStatus', errcheck)
        self.GetTime = ffi('GetTime', errcheck)
        self.SetTime = ffi('SetTime', errcheck)
        self.GetListener = ffi('GetListener', errcheck)
        self.SetListener = ffi('SetListener', errcheck)
        self.GetDoorControl = ffi('GetDoorControl', errcheck)
        self.SetDoorControl = ffi('SetDoorControl', errcheck)
        self.OpenDoor = ffi('OpenDoor', errcheck)
        self.GetCards = ffi('GetCards', errcheck)
        self.GetCard = ffi('GetCard', errcheck)
        self.GetCardByIndex = ffi('GetCardByIndex', errcheck)
        self.PutCard = ffi('PutCard', errcheck)
        self.DeleteCard = ffi('DeleteCard', errcheck)
        self.DeleteCards = ffi('DeleteCards', errcheck)
        self.GetEventIndex = ffi('GetEventIndex', errcheck)
        self.SetEventIndex = ffi('SetEventIndex', errcheck)
        self.GetEvent = ffi('GetEvent', errcheck)
        self.RecordSpecialEvents = ffi('RecordSpecialEvents', errcheck)
        self.GetTimeProfile = ffi('GetTimeProfile', errcheck)
        self.SetTimeProfile = ffi('SetTimeProfile', errcheck)


def ffi(tag, errcheck):
    (ff, argtypes) = libfunctions()[tag]

    ff.argtypes = argtypes
    ff.restype = ctypes.c_char_p
    ff.errcheck = errcheck

    return ff


# yapf: disable
@cache
def libfunctions():
    return {
        'GetDevices':          (lib.GetDevices,          [POINTER(GoUHPPOTE), POINTER(ctypes.c_int), POINTER(ctypes.c_uint32)]),
        'GetDevice':           (lib.GetDevice,           [POINTER(GoUHPPOTE), POINTER(GoDevice),  c_ulong]),
        'SetAddress':          (lib.SetAddress,          [POINTER(GoUHPPOTE), c_ulong, c_char_p, c_char_p, c_char_p]),
        'GetStatus':           (lib.GetStatus,           [POINTER(GoUHPPOTE), POINTER(GoStatus), c_ulong]),
        'GetTime':             (lib.GetTime,             [POINTER(GoUHPPOTE), POINTER(c_char_p), c_ulong]),
        'SetTime':             (lib.SetTime,             [POINTER(GoUHPPOTE), c_ulong, c_char_p]),
        'GetListener':         (lib.GetListener,         [POINTER(GoUHPPOTE), POINTER(c_char_p), c_ulong]),
        'SetListener':         (lib.SetListener,         [POINTER(GoUHPPOTE), c_ulong, c_char_p]),
        'GetDoorControl':      (lib.GetDoorControl,      [POINTER(GoUHPPOTE), POINTER(GoDoorControl), c_ulong, c_ubyte]),
        'SetDoorControl':      (lib.SetDoorControl,      [POINTER(GoUHPPOTE), c_ulong, c_ubyte, c_ubyte, c_ubyte]),
        'OpenDoor':            (lib.OpenDoor,            [POINTER(GoUHPPOTE), c_ulong, c_ubyte]),
        'GetCards':            (lib.GetCards,            [POINTER(GoUHPPOTE), POINTER(c_int), c_ulong]),
        'GetCard':             (lib.GetCard,             [POINTER(GoUHPPOTE), POINTER(GoCard), c_ulong, c_ulong]),
        'GetCardByIndex':      (lib.GetCardByIndex,      [POINTER(GoUHPPOTE), POINTER(GoCard), c_ulong, c_ulong]),
        'PutCard':             (lib.PutCard,             [POINTER(GoUHPPOTE), c_ulong, c_ulong, c_char_p, c_char_p, POINTER(c_ubyte)]),
        'DeleteCard':          (lib.DeleteCard,          [POINTER(GoUHPPOTE), c_ulong, c_ulong]),
        'DeleteCards':         (lib.DeleteCards,         [POINTER(GoUHPPOTE), c_ulong]),
        'GetEventIndex':       (lib.GetEventIndex,       [POINTER(GoUHPPOTE), POINTER(c_ulong), c_ulong]),
        'SetEventIndex':       (lib.SetEventIndex,       [POINTER(GoUHPPOTE), c_ulong, c_ulong]),
        'GetEvent':            (lib.GetEvent,            [POINTER(GoUHPPOTE), POINTER(GoEvent), c_ulong, c_ulong]),
        'RecordSpecialEvents': (lib.RecordSpecialEvents, [POINTER(GoUHPPOTE), c_ulong, c_bool]),
        'GetTimeProfile':      (lib.GetTimeProfile,      [POINTER(GoUHPPOTE), POINTER(GoTimeProfile), c_ulong, c_ubyte]),
        'SetTimeProfile':      (lib.SetTimeProfile,      [POINTER(GoUHPPOTE), c_ulong, POINTER(GoTimeProfile)]),
    }
# yapf: enable


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
        ('eventType', c_ubyte),
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


class GoTimeProfile(Structure):
    _fields_ = [
        ('ID', c_ubyte),
        ('linked', c_ubyte),
        ('start', c_char_p),
        ('end', c_char_p),
        ('monday', c_ubyte),
        ('tuesday', c_ubyte),
        ('wednesday', c_ubyte),
        ('thursday', c_ubyte),
        ('friday', c_ubyte),
        ('saturday', c_ubyte),
        ('sunday', c_ubyte),
        ('segment1start', c_char_p),
        ('segment1end', c_char_p),
        ('segment2start', c_char_p),
        ('segment2end', c_char_p),
        ('segment3start', c_char_p),
        ('segment3end', c_char_p),
    ]
