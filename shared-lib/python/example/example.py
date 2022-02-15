#!python

import argparse
import ctypes
import sys

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

            elif cmd == 'all':
                get_devices(u)
                get_device(u, 405419896)

        except BaseException as x:
            print()
            print(f"*** ERROR  {cmd} failed: {x}")
            print()

            sys.exit(1)
