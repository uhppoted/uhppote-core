#!python

import argparse
import ctypes
import sys

lib = ctypes.cdll.LoadLibrary("libuhppote.so")

class udevice(ctypes.Structure):
    _fields_ = [ ('id', ctypes.POINTER(ctypes.c_ulong)),
                 ('address', ctypes.POINTER(ctypes.c_char)),
                 ('next', ctypes.c_void_p),
               ]

class UHPPOTE(ctypes.Structure):
    _fields_ = [ ('bind', ctypes.POINTER(ctypes.c_char)),
                 ('broadcast', ctypes.POINTER(ctypes.c_char)),
                 ('listen', ctypes.POINTER(ctypes.c_char)),
                 ('timeout', ctypes.c_int),
                 ('devices', ctypes.POINTER(udevice)),
                 ('debug', ctypes.c_bool),
               ]

    def __init__(self, bind, broadcast,listen,timeout, debug):
      super(UHPPOTE,self).__init__()
      self.bind = ctypes.create_string_buffer(bind,len(bind))
      self.broadcast = ctypes.create_string_buffer(broadcast,len(broadcast))
      self.listen = ctypes.create_string_buffer(listen,len(listen))
      self.timeout = timeout
      self.devices = None
      self.debug = debug

class Device(ctypes.Structure):
    _fields_ = [ ('ID', ctypes.c_ulong),
                 ('address', ctypes.POINTER(ctypes.c_char)),
                 ('subnet', ctypes.POINTER(ctypes.c_char)),
                 ('gateway', ctypes.POINTER(ctypes.c_char)),
                 ('MAC', ctypes.POINTER(ctypes.c_char)),
                 ('version', ctypes.POINTER(ctypes.c_char)),
                 ('date', ctypes.POINTER(ctypes.c_char)),
               ]

class GetDeviceResult(ctypes.Structure):
    _fields_ = [ ('r0', Device),
                 ('r1', ctypes.POINTER(ctypes.c_char))
               ]

def usage():
    print()
    print("  Usage: python example.py <command>")
    print()

def help():
    print("")
    print("Usage: python example.py <command>")
    print("")
    print("  commands")
    print("    get-devices")
    print("    get-device")
    print("    help")
    print("")
    print("  get-devices")
    print("    Retrieves a list of UHPPOTE controller IDs findable on the local LAN.")
    print("")
    print("  get-device")
    print("    Retrieves the basic device information for a single UHPPOTE controller.")
    print("")
    print("  help")
    print("    Displays this information.")
    print("")


def get_devices():
    print("get-devices")

def get_device():
    print("get-device")
    GetDevice = lib.GetDevice
    GetDevice.argtypes = [ctypes.POINTER(UHPPOTE),ctypes.c_ulong]
    GetDevice.restype = (GetDeviceResult)

    u = UHPPOTE(b'192.168.1.100', b'192.168.1.255', b'192.168.1.100:60001', 2, True)

    result = lib.GetDevice(u, 405419896)
    print("  ID:      " + str(result.r0.ID))
    print("  IP:      " + str(ctypes.cast(result.r0.address,ctypes.c_char_p).value))
    print("           " + str(ctypes.cast(result.r0.subnet,ctypes.c_char_p).value))
    print("           " + str(ctypes.cast(result.r0.gateway,ctypes.c_char_p).value))
    print("  MAC:     " + str(ctypes.cast(result.r0.MAC,ctypes.c_char_p).value))
    print("  version: " + str(ctypes.cast(result.r0.version,ctypes.c_char_p).value))
    print("  date:    " + str(ctypes.cast(result.r0.date,ctypes.c_char_p).value))
    print(ctypes.cast(result.r1,ctypes.c_char_p).value)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Example CLI for the uhppote-core Python integration')
    
    parser.add_argument("command", default="help")

    options = parser.parse_args()
    cmd = options.command

    if cmd == "help":
        help()

    elif cmd not in [ 'get-devices',
                      'get-device',
                    ]:
        print()
        print(f"  ERROR: invalid command ({cmd}). Try 'help' to see all commands and options")
        print()

    else:
        try:            
            if cmd == 'get-devices':
                get_devices()
    
            elif cmd == 'get-device':
                get_device()
    
        except BaseException as x:
            print()
            print(f"*** ERROR  {cmd} failed: {x}")
            print()

            sys.exit(1)
