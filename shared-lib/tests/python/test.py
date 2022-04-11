#!python

import ctypes
import sys

sys.path.append('../../bindings/python')

import uhppoted

from uhppoted import NORMALLY_OPEN
from uhppoted import NORMALLY_CLOSED
from uhppoted import CONTROLLED

DEVICE_ID = 405419896
CARD_NUMBER = 8165538
CARD_INDEX = 7
DOOR = 4


def tests():
    return {
        'get-devices': get_devices,
        'get-device': get_device,
        'set-address': set_address,
        'get-status': get_status,
        'get-time': get_time,
        'set-time': set_time,
        'get-listener': get_listener,
        'set-listener': set_listener,
        'get-door-control': get_door_control,
        'set-door-control': set_door_control,
        'get-cards': get_cards,
        'get-card': get_card,
        'get-card-by-index': get_card_by_index,
        'put-card': put_card,
        'delete-card': delete_card,
    }


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
        print(f"get-devices       ok")

    return ok


def get_device(u):
    info = u.get_device(DEVICE_ID)
    ok = True

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
        print(f"get-device        ok")

    return ok


def set_address(u):
    u.set_address(DEVICE_ID, "192.168.1.125", "255.255.255.253", "192.168.1.5")
    ok = True

    if ok:
        print(f"set-address       ok")

    return ok


def get_status(u):
    status = u.get_status(DEVICE_ID)
    ok = True

    if status.ID != 405419896:
        print(f"get-status: incorrect device ID - expected:405419896, got:{status.ID}")
        ok = False

    if status.sysdatetime != "2022-03-19 15:48:32":
        print(f"get-status: incorrect system date/time - expected:2022-03-19 15:48:32, got:{status.sysdatetime}")
        ok = False

    if status.doors[0] != 1 or status.doors[1] != 0 or status.doors[2] != 0 or status.doors[3] != 1:
        print(
            f"get-status: incorrect doors state - expected:[1,0,0,1], got:[{status.doors[0]},{status.doors[1]},{status.doors[2]},{status.doors[3]}]"
        )
        ok = False

    if status.buttons[0] != 1 or status.buttons[1] != 0 or status.buttons[2] != 1 or status.buttons[3] != 0:
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
        print(f"get-status: incorrect event timestamp - expected:2022-01-02 12:34:56, got:{status.event.timestamp}")
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
        print(f"get-status        ok")

    return ok


def get_time(u):
    datetime = u.get_time(DEVICE_ID)
    ok = True

    if datetime != "2022-01-02 12:34:56":
        print(f"get-time: incorrect date/time - expected:2022-01-02 12:34:56, got:{datetime}")
        ok = False

    if ok:
        print(f"get-time          ok")

    return ok


def set_time(u):
    u.set_time(DEVICE_ID, '2022-03-23 12:24:17')
    ok = True

    if ok:
        print(f"set-time          ok")

    return ok


def get_listener(u):
    listener = u.get_listener(DEVICE_ID)
    ok = True

    if listener != "192.168.1.100:60001":
        print(f"get-listener: incorrect event listener - expected:192.168.1.100:60001, got:{listener}")
        ok = False

    if ok:
        print(f"get-listener      ok")

    return ok


def set_listener(u):
    u.set_listener(DEVICE_ID, '192.168.1.100:60001')
    ok = True

    if ok:
        print(f"set-listener      ok")

    return ok


def get_door_control(u):
    control = u.get_door_control(DEVICE_ID, DOOR)
    ok = True

    if control.mode != CONTROLLED:
        print(f"get-door-control: incorrect door control mode - expected:3, got:{control.mode}")
        ok = False

    if control.delay != 7:
        print(f"get-door-control: incorrect door open delay - expected:7, got:{control.delay}")
        ok = False

    if ok:
        print(f"get-door-control  ok")

    return ok


def set_door_control(u):
    u.set_door_control(DEVICE_ID, DOOR, NORMALLY_CLOSED, 6)

    if ok:
        print(f"set-door-control  ok")

    return True


def get_cards(u):
    cards = u.get_cards(DEVICE_ID)
    ok = True

    if cards != 39:
        print(f"get-cards: incorrect card count - expected:39, got:{cards}")
        ok = False

    if ok:
        print(f"get-cards         ok")

    return ok


def get_card(u):
    card = u.get_card(DEVICE_ID, CARD_NUMBER)
    ok = True

    if card.cardNumber != 8165538:
        print(f"get-card: incorrect card number - expected:8165538, got:{card.cardNumber}")
        ok = False

    if card.start != '2022-01-01':
        print(f"get-card: incorrect 'from' date - expected:2022-01-01, got:{card.start}")
        ok = False

    if card.end != '2022-12-31':
        print(f"get-card: incorrect 'to' date - expected:2022-12-31, got:{card.end}")
        ok = False

    if card.doors[0] != 0:
        print(f"get-card: incorrect door[1] - expected:0, got:{card.doors[0]}")
        ok = False

    if card.doors[1] != 1:
        print(f"get-card: incorrect door[2] - expected:1, got:{card.doors[1]}")
        ok = False

    if card.doors[2] != 31:
        print(f"get-card: incorrect door[3] - expected:31, got:{card.doors[2]}")
        ok = False

    if card.doors[3] != 75:
        print(f"get-card: incorrect door[42] - expected:75, got:{card.doors[3]}")
        ok = False

    if ok:
        print(f"get-card          ok")

    return ok


def get_card_by_index(u):
    card = u.get_card_by_index(DEVICE_ID, CARD_INDEX)
    ok = True

    if card.cardNumber != 8165538:
        print(f"get-card_by_index: incorrect card number - expected:8165538, got:{card.cardNumber}")
        ok = False

    if card.start != '2022-01-01':
        print(f"get-card_by_index: incorrect 'from' date - expected:2022-01-01, got:{card.start}")
        ok = False

    if card.end != '2022-12-31':
        print(f"get-card_by_index: incorrect 'to' date - expected:2022-12-31, got:{card.end}")
        ok = False

    if card.doors[0] != 0:
        print(f"get-card_by_index: incorrect door[1] - expected:0, got:{card.doors[0]}")
        ok = False

    if card.doors[1] != 1:
        print(f"get-card_by_index: incorrect door[2] - expected:1, got:{card.doors[1]}")
        ok = False

    if card.doors[2] != 31:
        print(f"get-card_by_index: incorrect door[3] - expected:31, got:{card.doors[2]}")
        ok = False

    if card.doors[3] != 75:
        print(f"get-card_by_index: incorrect door[42] - expected:75, got:{card.doors[3]}")
        ok = False

    if ok:
        print(f"get-card_by_index ok")

    return ok


def put_card(u):
    u.put_card(DEVICE_ID, CARD_NUMBER, '2022-01-01', '2022-12-31', [0, 1, 31, 75])

    return result('put-card', True)


def delete_card(u):
    u.delete_card(DEVICE_ID, CARD_NUMBER)

    return result('delete-card', True)


def result(test, ok):
    if ok:
        print(f'{test:<17} ok')

    return ok


def usage():
    print('   Usage: python test.py <command>')
    print()
    print('   Supported commands:')
    print('      all')
    for k in tests():
        print(f'      {k}')
    print()


if __name__ == "__main__":
    cmd = ''

    if len(sys.argv) > 1:
        cmd = sys.argv[1]

    alpha = uhppoted.Controller(405419896, '192.168.1.100')
    beta = uhppoted.Controller(303986753, '192.168.1.100')
    controllers = [alpha, beta]

    u = uhppoted.Uhppote(
        uhppote=uhppoted.UHPPOTE('192.168.1.100', '192.168.1.255', '192.168.1.100:60001', 2500, controllers, True))
    ok = True

    try:
        if cmd in tests():
            ok = ok if tests()[cmd](u) else False

        elif cmd == '' or cmd == 'all':
            for _, f in tests().items():
                ok = ok if f(u) else False

        elif cmd == 'help':
            print()
            usage()

        else:
            print()
            print(f'   *** ERROR invalid command ({cmd})')
            print()
            usage()

        if not ok:
            sys.exit(-1)

    except BaseException as x:
        print()
        print(f'*** ERROR  {cmd} failed: {x}')
        print()

        sys.exit(1)
