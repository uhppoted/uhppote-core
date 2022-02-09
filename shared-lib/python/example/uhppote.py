import ctypes
import platform

from dataclasses import dataclass


if 'Windows' in platform.system():         
    lib = ctypes.windll.LoadLibrary("../../lib/uhppote.dll")
else:
    lib = ctypes.cdll.LoadLibrary("../../lib/libuhppote.so")


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
class UHPPOTE():
    bind: str = ''
    broadcast: str = ''
    listen: str = ''
    timeout: int = 5
    debug: int = False
    
class Uhppote:  
    def __init__(self, uhppote=None):
      self._uhppote = None
      if uhppote:
        self._uhppote = GoUHPPOTE(uhppote.bind,
                                  uhppote.broadcast,
                                  uhppote.listen,
                                  uhppote.timeout,
                                  uhppote.debug)

    def get_devices(self):
      print("get-devices")

    def get_device(self, deviceID):
        GetDevice = lib.GetDevice
        GetDevice.argtypes = [ctypes.POINTER(GoUHPPOTE),ctypes.c_ulong]
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
class GoController(ctypes.Structure):
    _fields_ = [ ('id', ctypes.c_ulong),
                 ('address', ctypes.c_char_p),
                 ('next', ctypes.c_void_p),
               ]

class GoUHPPOTE(ctypes.Structure):
    _fields_ = [ ('bind', ctypes.c_char_p),
                 ('broadcast', ctypes.c_char_p),
                 ('listen', ctypes.c_char_p),
                 ('timeout', ctypes.c_int),
                 ('devices', ctypes.POINTER(GoController)),
                 ('debug', ctypes.c_bool),
               ]

    def __init__(self,bind,broadcast,listen,timeout,debug):
      super(GoUHPPOTE,self).__init__()
      self.bind = ctypes.c_char_p(bytes(bind, 'utf-8'))
      self.broadcast = ctypes.c_char_p(bytes(broadcast, 'utf-8'))
      self.listen = ctypes.c_char_p(bytes(listen, 'utf-8'))
      self.timeout = timeout
      self.devices = None
      self.debug = debug

class GoDevice(ctypes.Structure):
    _fields_ = [ ('ID', ctypes.c_ulong),
                 ('address', ctypes.c_char_p),
                 ('subnet', ctypes.c_char_p),
                 ('gateway', ctypes.c_char_p),
                 ('MAC', ctypes.c_char_p),
                 ('version', ctypes.c_char_p),
                 ('date', ctypes.c_char_p),
               ]

class GoGetDeviceResult(ctypes.Structure):
    _fields_ = [ ('r0', GoDevice),
                 ('r1', ctypes.c_char_p)
               ]

