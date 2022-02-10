import ctypes
import platform

from ctypes import c_bool
from ctypes import c_char_p
from ctypes import c_int
from ctypes import c_longlong
from ctypes import c_ulong
from ctypes import pointer
from ctypes import Structure
from ctypes import POINTER

from dataclasses import dataclass

if 'Windows' in platform.system():         
    lib = ctypes.windll.LoadLibrary("uhppote.dll")
else:
    lib = ctypes.cdll.LoadLibrary("libuhppote.so")

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
    debug: int
    
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
            self._uhppote = GoUHPPOTE(uhppote.bind,
                                      uhppote.broadcast,
                                      uhppote.listen,
                                      uhppote.timeout,
                                      uhppote.controllers,
                                      uhppote.debug)

    def get_devices(self):
        GetDevices = lib.GetDevices
        GetDevices.argtypes = [POINTER(GoUHPPOTE),GoUint32Slice]
        GetDevices.restype = (GoGetDevicesResult)

        N = 0

        while True:
            N = N + 16
            slice = GoUint32Slice((c_ulong * N)(*[0] * N), N, N) 
            result = lib.GetDevices(self._uhppote, slice)

            if result.r1:
                raise Exception(f"{result.r1.decode('utf-8')}")
            elif result.r0 <= N:
                break

        return slice.data[0:result.r0]

    def get_device(self, deviceID):
        GetDevice = lib.GetDevice
        GetDevice.argtypes = [POINTER(GoUHPPOTE),c_ulong]
        GetDevice.restype = (GoGetDeviceResult)

        result = lib.GetDevice(self._uhppote, deviceID)

        if result.r1:
            raise Exception(f"{result.r1.decode('utf-8')}")
        else:
          return Device(result.r0.ID,
                        result.r0.address.decode('utf-8'),
                        result.r0.subnet.decode('utf-8'),
                        result.r0.gateway.decode('utf-8'),
                        result.r0.MAC.decode('utf-8'),
                        result.r0.version.decode('utf-8'),
                        result.r0.date.decode('utf-8'))

# INTERNAL TYPES

class GoUint32Slice(Structure):
    _fields_ = [ ('data', POINTER(c_ulong)),
                 ('len', c_longlong),
                 ('cap', c_longlong),
               ]

    # _fields_ = [ ('id', c_ulong),
    #              ('address', c_char_p)
    #            ]
class GoController(Structure):
    pass
    
GoController._fields_ = [ ('id', c_ulong),
                          ('address', c_char_p),
                          ('next', POINTER(GoController))
                       ]

class GoUHPPOTE(Structure):
    _fields_ = [ ('bind', c_char_p),
                 ('broadcast', c_char_p),
                 ('listen', c_char_p),
                 ('timeout', c_int),
                 ('devices', POINTER(GoController)),
                 ('debug', c_bool)
               ]

    def __init__(self,bind,broadcast,listen,timeout,controllers,debug):
      super(GoUHPPOTE,self).__init__()
      self.bind = c_char_p(bytes(bind, 'utf-8'))
      self.broadcast = c_char_p(bytes(broadcast, 'utf-8'))
      self.listen = c_char_p(bytes(listen, 'utf-8'))
      self.timeout = timeout
      self.devices = None
      self.debug = debug

      p = None
      for c in controllers:
          cc = GoController()
          cc.id = c_ulong(c.id)
          cc.address = c_char_p(bytes(c.address, 'utf-8'))
          cc.next = p
          p = pointer(cc)

      self.devices = p

class GoDevice(Structure):
    _fields_ = [ ('ID', c_ulong),
                 ('address', c_char_p),
                 ('subnet', c_char_p),
                 ('gateway', c_char_p),
                 ('MAC', c_char_p),
                 ('version', c_char_p),
                 ('date', c_char_p),
               ]

class GoGetDevicesResult(Structure):
    _fields_ = [ ('r0', c_int),
                 ('r1', c_char_p)
               ]

class GoGetDeviceResult(Structure):
    _fields_ = [ ('r0', GoDevice),
                 ('r1', c_char_p)
               ]

