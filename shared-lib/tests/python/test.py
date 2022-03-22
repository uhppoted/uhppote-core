#!python

import argparse
import ctypes
import sys

sys.path.append('../../bindings/python')

import uhppoted


def get_devices(u):
    devices = u.get_devices()
    ok = True

    if len(devices) != 3:
        print(f"get-devices: incorrect device count - expected:3, got:{len(devices)}")
        ok = False
    elif (devices[0] != 201020304 or devices[1] != 303986753 or devices[2] != 405419896):
        print(
            f"get-devices: incorrect device list - expected:[ 201020304,303986753,405419896 ], got:[ {devices[0]},{devices[1]},{devices[2]} ]"
        )
        ok = False

    if ok:
        print(f"get-devices: ok")

    return ok


def get_device(u, deviceID):
    info = u.get_device(deviceID)
    ok = True

    print(info)
    if info.ID != 405419896:
        print(f"get-device: incorrect device ID - expected:405419896, got:{info.ID}")
        ok = False

    if info.address != "192.168.1.101":
        print(f"get-device: incorrect IP address - expected:192.168.1.101, got:{info.address}")
        ok = False

    if info.subnet != "255.255.255.0":
        print(f"get-device: incorrect subnet mask - expected:255.255.255.0, got:{info.subnet}")
        ok = False

    if info.gateway != "192.168.1.1":
        print(f"get-device: incorrect gateway address - expected:192.168.1.1, got:{info.gateway}")
        ok = False

    if info.MAC != "00:12:23:34:45:56":
        print(f"get-device: incorrect MAC address - expected:00:12:23:34:45:56, got:{info.MAC}")
        ok = False

    if info.version != "v8.92":
        print(f"get-device: incorrect version - expected:v8.92, got:{info.version}")
        ok = False

    if info.date != "2018-11-05":
        print(f"get-device: incorrect date - expected:2018-11-05, got:{info.date}")
        ok = False

    if ok:
        print(f"get-device:  ok")

    return ok


def set_address(u, deviceID, address, subnet, gateway):
    u.set_address(deviceID, address, subnet, gateway)
    ok = True

    if ok:
        print(f"set-address: ok")

    return ok


def get_status(u, deviceID):
    status = u.get_status(deviceID)
    ok = True

    if status.ID != 405419896:
        print(f"get-status: incorrect device ID - expected:405419896, got:{status.ID}")
        ok = False

    if status.sysdatetime != "2022-03-19 15:48:32":
        print(
            f"get-status: incorrect system date/time - expected:2022-03-19 15:48:32, got:{status.sysdatetime}"
        )
        ok = False

    if status.doors[0] != 1 or status.doors[1] != 0 or status.doors[2] != 0 or status.doors[3] != 1:
        print(
            f"get-status: incorrect doors state - expected:[1,0,0,1], got:[{status.doors[0]},{status.doors[1]},{status.doors[2]},{status.doors[3]}]"
        )
        ok = False

    if status.buttons[0] != 1 or status.buttons[1] != 0 or status.buttons[2] != 1 or status.buttons[
            3] != 0:
        print(
            f"get-status: incorrect buttons state - expected:[1,0,1,0], got:[{status.buttons[0]},{status.buttons[1]},{status.buttons[2]},{status.buttons[3]}]"
        )
        ok = False

    if status.relays != 0x12:
        print(f"get-status: incorrect relay state - expected:0x12, got:{status.relays}")
        ok = False

    if status.inputs != 0x34:
        print(f"get-status: incorrect inputs state - expected:0x34, got:{status.inputs}")
        ok = False

    if status.syserror != 0x56:
        print(f"get-status: incorrect system error - expected:0x56, got:{status.syserror}")
        ok = False

    if status.info != 253:
        print(f"get-status: incorrect special info - expected:253, got:{status.info}")
        ok = False

    if status.seqno != 9876:
        print(f"get-status: incorrect sequence number - expected:9876, got:{status.seqno}")
        ok = False

    if status.event.timestamp != "2022-01-02 12:34:56":
        print(
            f"get-status: incorrect event timestamp - expected:2022-01-02 12:34:56, got:{status.event.timestamp}"
        )
        ok = False

    if status.event.index != 135:
        print(f"get-status: incorrect event index - expected:135, got:{status.event.index}")
        ok = False

    if status.event.type != 6:
        print(f"get-status: incorrect event type - expected:6, got:{status.event.type}")
        ok = False

    if status.event.granted != 1:
        print(f"get-status: incorrect event granted - expected:1, got:{status.event.granted}")
        ok = False

    if status.event.door != 3:
        print(f"get-status: incorrect event door - expected:3, got:{status.event.door}")
        ok = False

    if status.event.direction != 1:
        print(f"get-status: incorrect event direction - expected:1, got:{status.event.direction}")
        ok = False

    if status.event.card != 8100023:
        print(f"get-status: incorrect event card - expected:8100023, got:{status.event.card}")
        ok = False

    if status.event.reason != 21:
        print(f"get-status: incorrect event reason - expected:21, got:{status.event.reason}")
        ok = False

    if ok:
        print(f"get-status:  ok")

    return ok


def get_time(u, deviceID):
    datetime = u.get_time(deviceID)
    ok = True

    if datetime != "2022-01-02 12:34:56":
        print(f"get-time: incorrect date/time - expected:2022-01-02 12:34:56, got:{datetime}")
        ok = False

    if ok:
        print(f"get-time:    ok")

    return ok


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Example CLI for the uhppote-core Python integration')

    parser.add_argument("command", default="all")

    options = parser.parse_args()
    cmd = options.command

    alpha = uhppoted.Controller(405419896, "192.168.1.100")
    beta = uhppoted.Controller(303986753, "192.168.1.100")
    controllers = [alpha, beta]

    u = uhppoted.Uhppote(uhppote=uhppoted.UHPPOTE('192.168.1.100', '192.168.1.255',
                                                  '192.168.1.100:60001', 1, controllers, True))
    ok = True

    try:
        if cmd == 'get-devices':
            ok = ok if get_devices(u) else False

        elif cmd == 'get-device':
            ok = ok if get_device(u, 405419896) else False

        elif cmd == 'set-address':
            ok = set_address(u, 405419896, "192.168.1.125", "255.255.255.253", "192.168.1.5")

        elif cmd == 'get-status':
            ok = get_status(u, 405419896)

        elif cmd == 'get-time':
            ok = get_time(u, 405419896)

        elif cmd == 'all':
            ok = ok if get_devices(u) else False
            ok = ok if get_device(u, 405419896) else False
            ok = ok if set_address(u, 405419896, "192.168.1.125", "255.255.255.253",
                                   "192.168.1.5") else False
            ok = ok if get_status(u, 405419896) else False
            ok = ok if get_time(u, 405419896) else False

        if not ok:
            sys.exit(-1)

    except BaseException as x:
        print()
        print(f"*** ERROR  {cmd} failed: {x}")
        print()

        sys.exit(1)
