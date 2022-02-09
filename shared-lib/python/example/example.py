#!python

import argparse
import ctypes
import sys
import uhppote

# if 'Windows' in system:         
#     lib = ctypes.windll.LoadLibrary("../../lib/uhppote.dll")
# else:
#     lib = ctypes.cdll.LoadLibrary("../../lib/libuhppote.so")

# class udevice(ctypes.Structure):
#     _fields_ = [ ('id', ctypes.c_ulong),
#                  ('address', ctypes.c_char_p),
#                  ('next', ctypes.c_void_p),
#                ]

# class UHPPOTE(ctypes.Structure):
#     _fields_ = [ ('bind', ctypes.c_char_p),
#                  ('broadcast', ctypes.c_char_p),
#                  ('listen', ctypes.c_char_p),
#                  ('timeout', ctypes.c_int),
#                  ('devices', ctypes.POINTER(udevice)),
#                  ('debug', ctypes.c_bool),
#                ]

#     def __init__(self, bind, broadcast,listen,timeout, debug):
#       super(UHPPOTE,self).__init__()
#       self.bind = ctypes.c_char_p(bytes(bind, 'utf-8'))
#       self.broadcast = ctypes.c_char_p(bytes(broadcast, 'utf-8'))
#       self.listen = ctypes.c_char_p(bytes(listen, 'utf-8'))
#       self.timeout = timeout
#       self.devices = None
#       self.debug = debug

# class Device(ctypes.Structure):
#     _fields_ = [ ('ID', ctypes.c_ulong),
#                  ('address', ctypes.c_char_p),
#                  ('subnet', ctypes.c_char_p),
#                  ('gateway', ctypes.c_char_p),
#                  ('MAC', ctypes.c_char_p),
#                  ('version', ctypes.c_char_p),
#                  ('date', ctypes.c_char_p),
#                ]

# class GetDeviceResult(ctypes.Structure):
#     _fields_ = [ ('r0', Device),
#                  ('r1', ctypes.c_char_p)
#                ]

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


def get_devices(u):
    print("get-devices")

def get_device(u, deviceID):
    print("get-device")
    try:
        info = u.get_device(deviceID)

        print(f"  ID:      {info.ID}")
        print(f"  IP:      {info.address}  {info.subnet}  {info.gateway}")
        print(f"  MAC:     {info.MAC}")
        print(f"  version: {info.version}")
        print(f"  date:    {info.date}")
        print()
    except Exception as e:
        print(f" *** ERROR get-device ({e})")
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
        u = uhppote.Uhppote(uhppote=uhppote.UHPPOTE('192.168.1.100', '192.168.1.255', '192.168.1.100:60001', 1, True))
        try:            
            if cmd == 'get-devices':
                get_devices(u)
    
            elif cmd == 'get-device':
                get_device(u, 405419896)
    
        except BaseException as x:
            print()
            print(f"*** ERROR  {cmd} failed: {x}")
            print()

            sys.exit(1)
