#!python

import argparse
import ctypes
import sys

lib = ctypes.cdll.LoadLibrary("../../lib/libuhppote.so")

class udevice(ctypes.Structure):
    _fields_ = [ ('id', ctypes.c_ulong),
                 ('address', ctypes.c_char_p),
                 ('next', ctypes.c_void_p),
               ]

class UHPPOTE(ctypes.Structure):
    _fields_ = [ ('bind', ctypes.c_char_p),
                 ('broadcast', ctypes.c_char_p),
                 ('listen', ctypes.c_char_p),
                 ('timeout', ctypes.c_int),
                 ('devices', ctypes.POINTER(udevice)),
                 ('debug', ctypes.c_bool),
               ]

    def __init__(self, bind, broadcast,listen,timeout, debug):
      super(UHPPOTE,self).__init__()
      self.bind = ctypes.c_char_p(bytes(bind, 'utf-8'))
      self.broadcast = ctypes.c_char_p(bytes(broadcast, 'utf-8'))
      self.listen = ctypes.c_char_p(bytes(listen, 'utf-8'))
      self.timeout = timeout
      self.devices = None
      self.debug = debug

class Device(ctypes.Structure):
    _fields_ = [ ('ID', ctypes.c_ulong),
                 ('address', ctypes.c_char_p),
                 ('subnet', ctypes.c_char_p),
                 ('gateway', ctypes.c_char_p),
                 ('MAC', ctypes.c_char_p),
                 ('version', ctypes.c_char_p),
                 ('date', ctypes.c_char_p),
               ]

class GetDeviceResult(ctypes.Structure):
    _fields_ = [ ('r0', Device),
                 ('r1', ctypes.c_char_p)
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

def get_device(deviceID):
    GetDevice = lib.GetDevice
    GetDevice.argtypes = [ctypes.POINTER(UHPPOTE),ctypes.c_ulong]
    GetDevice.restype = (GetDeviceResult)

    u = UHPPOTE('192.168.1.100', '192.168.1.255', '192.168.1.100:60001', 2, True)

    result = lib.GetDevice(u, deviceID)

    print("get-device")

    if result.r1:
        print("** ERROR:" + result.r1.decode('utf-8'))
    else:
        print(f"  ID:      {result.r0.ID}")
        print(f"  IP:      {result.r0.address.decode('utf-8')}  {result.r0.subnet.decode('utf-8')}  {result.r0.gateway.decode('utf-8')}")
        print(f"  MAC:     {result.r0.MAC.decode('utf-8')}")
        print(f"  version: {result.r0.version.decode('utf-8')}")
        print(f"  date:    {result.r0.date.decode('utf-8')}")

    print()

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
                get_device(405419896)
    
        except BaseException as x:
            print()
            print(f"*** ERROR  {cmd} failed: {x}")
            print()

            sys.exit(1)
