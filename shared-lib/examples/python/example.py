#!python

import argparse
import ctypes
import sys

sys.path.append('../../bindings/python')

import uhppoted


def usage():
    print()
    print("  Usage: python example.py <command>")
    print()
    print("         python example.py help for a list of commands")
    print()


def help():
    print()
    print("Usage: python example.py <command>")
    print()
    print("  commands")
    print("    get-devices")
    print("    get-device")
    print("    set-device")
    print("    get-status")
    print("    help")
    print()
    print("  get-devices")
    print(
        "    Retrieves a list of UHPPOTE controller IDs findable on the local LAN."
    )
    print()
    print("  get-device")
    print(
        "    Retrieves the basic device information for a single UHPPOTE controller."
    )
    print()
    print("  set-address")
    print(
        "    Sets the controller IPv4 address, subnet mask and gateway address."
    )
    print()
    print("  get-status")
    print(
        "    Retrieves the controller status for a single UHPPOTE controller.")
    print()
    print("  help")
    print("    Displays this information.")
    print()


def get_devices(u):
    try:
        list = u.get_devices()

        print(f"get-devices ({len(list)})")
        for id in list:
            print(f"  {id}")
        print()

    except Exception as e:
        print(f" *** ERROR get-devices ({e})")
        print()


def get_device(u, deviceID):
    try:
        info = u.get_device(deviceID)

        print("get-device")
        print(f"  ID:       {info.ID}")
        print(f"  IP:       {info.address}  {info.subnet}  {info.gateway}")
        print(f"  MAC:      {info.MAC}")
        print(f"  version:  {info.version}")
        print(f"  released: {info.date}")
        print()

    except Exception as e:
        print(f" *** ERROR get-device ({e})")
        print()


def set_address(u, deviceID, address, subnet, gateway):
    try:
        u.set_address(deviceID, address, subnet, gateway)

        print("set-address")
        print(f"  ID:      {deviceID}")
        print(f"  address: {address}")
        print(f"  subnet:  {subnet}")
        print(f"  gateway: {gateway}")
        print()

    except Exception as e:
        print(f" *** ERROR set-address ({e})")
        print()


def get_status(u, deviceID):
    try:
        status = u.get_status(deviceID)

        print("get-status")
        print(f"  ID:        {status.ID}")
        print(f"  date/time: {status.sysdatetime}")
        print(
            f"  doors:     {status.doors[0]} {status.doors[1]} {status.doors[2]} {status.doors[3]}"
        )
        print(
            f"  buttons:   {status.buttons[0]} {status.buttons[1]} {status.buttons[2]} {status.buttons[3]}"
        )
        print(f"  relays   : " + "{0:#0{1}x}".format(status.relays, 4))
        print(f"  inputs   : " + "{0:#0{1}x}".format(status.inputs, 4))
        print(f"  error    : " + "{0:#0{1}x}".format(status.syserror, 4))
        print(f"  info     : " + "{0:#0{1}x}".format(status.info, 4))
        print(f"  seq no.  : {status.seqno}")
        print(f"  event timestamp: {status.event.timestamp}")
        print(f"        index:     {status.event.index}")
        print(f"        type:      {status.event.type}")
        print(f"        granted:   {status.event.granted}")
        print(f"        door:      {status.event.door}")
        print(f"        direction: {status.event.direction}")
        print(f"        card:      {status.event.card}")
        print(f"        reason:    {status.event.reason}")
        print()

    except Exception as e:
        print(f" *** ERROR get-status ({e})")
        print()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Example CLI for the uhppote-core Python integration')

    parser.add_argument("command", default="help")

    options = parser.parse_args()
    cmd = options.command

    if cmd == "help":
        help()

    elif cmd not in [
            'get-devices',
            'get-device',
            'set-address',
            'get-status',
            'all',
    ]:
        print()
        print(
            f"  ERROR: invalid command ({cmd}). Try 'help' to see all commands and options"
        )
        print()

    else:
        alpha = uhppoted.Controller(405419896, "192.168.1.100")
        beta = uhppoted.Controller(303986753, "192.168.1.100")
        controllers = [alpha, beta]

        u = uhppoted.Uhppote(uhppote=uhppoted.UHPPOTE(
            '192.168.1.100', '192.168.1.255', '192.168.1.100:60001', 1,
            controllers, True))
        try:
            if cmd == 'get-devices':
                get_devices(u)

            elif cmd == 'get-device':
                get_device(u, 405419896)

            elif cmd == 'set-address':
                set_address(u, 405419896, "192.168.1.125", "255.255.255.253",
                            "192.168.1.5")

            elif cmd == 'get-status':
                get_status(u, 405419896)

            elif cmd == 'all':
                get_devices(u)
                get_device(u, 405419896)
                set_address(u, 405419896, "192.168.1.125", "255.255.253",
                            "192.168.1.5")
                get_status(u, 405419896)

        except BaseException as x:
            print()
            print(f"*** ERROR  {cmd} failed: {x}")
            print()

            sys.exit(1)