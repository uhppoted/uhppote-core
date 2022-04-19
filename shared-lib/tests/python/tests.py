#!python

import ctypes
import sys

from functools import reduce

sys.path.append('../../bindings/python')

import uhppoted

from uhppoted import NORMALLY_OPEN
from uhppoted import NORMALLY_CLOSED
from uhppoted import CONTROLLED

DEVICE_ID = 405419896
CARD_NUMBER = 8165538
CARD_INDEX = 19
EVENT_INDEX = 51
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
        'open-door': open_door,
        'get-cards': get_cards,
        'get-card': get_card,
        'get-card-by-index': get_card_by_index,
        'put-card': put_card,
        'delete-card': delete_card,
        'delete-cards': delete_cards,
        'get-event-index': get_event_index,
        'set-event-index': set_event_index,
        'get-event': get_event,
        'record-special-events': record_special_events,
    }


def get_devices(u):
    tag = 'get-devices'
    devices = u.get_devices()

    return evaluate(tag, [
        ('device count', 3, len(devices)),
        ('device list', [201020304, 303986753, 405419896], [devices[0], devices[1], devices[2]]),
    ])


def get_device(u):
    tag = 'get-device'
    info = u.get_device(DEVICE_ID)

    return evaluate(tag, [
        ('device ID', 405419896, info.ID),
        ('IP address', '192.168.1.101', info.address),
        ('subnet mask', '255.255.255.0', info.subnet),
        ('gateway address', '192.168.1.1', info.gateway),
        ('MAC address', '00:12:23:34:45:56', info.MAC),
        ('version', 'v8.92', info.version),
        ('date', '2018-11-05', info.date),
    ])


def set_address(u):
    tag = 'set-address'
    u.set_address(DEVICE_ID, '192.168.1.125', '255.255.254.0', '192.168.1.0')

    return evaluate(tag, [])


def get_status(u):
    tag = 'get-status'
    status = u.get_status(DEVICE_ID)

    return evaluate(tag, [
        ('device ID', 405419896, status.ID),
        ('system date/time', '2022-03-19 15:48:32', status.sysdatetime),
        ('doors state', [1, 0, 0, 1], [status.doors[0], status.doors[1], status.doors[2], status.doors[3]]),
        ('buttons state', [1, 0, 1, 0], [status.buttons[0], status.buttons[1], status.buttons[2], status.buttons[3]]),
        ('relay state', 0x12, status.relays),
        ('inputs state', 0x34, status.inputs),
        ('system error', 0x56, status.syserror),
        ('special info', 253, status.info),
        ('sequence number', 9876, status.seqno),
        ('event timestamp', '2022-01-02 12:34:56', status.event.timestamp),
        ('event index', 135, status.event.index),
        ('event type', 6, status.event.eventType),
        ('event granted', 1, status.event.granted),
        ('event door', 3, status.event.door),
        ('event direction', 1, status.event.direction),
        ('event card', 8100023, status.event.card),
        ('event reason', 21, status.event.reason),
    ])


def get_time(u):
    tag = 'get-time'
    datetime = u.get_time(DEVICE_ID)

    return evaluate(tag, [
        ('date/time', '2022-01-02 12:34:56', datetime),
    ])


def set_time(u):
    tag = 'set-time'
    u.set_time(DEVICE_ID, '2022-03-23 12:24:17')

    return evaluate(tag, [])


def get_listener(u):
    tag = 'get-listener'
    listener = u.get_listener(DEVICE_ID)

    return evaluate(tag, [
        ('event listener', '192.168.1.100:60001', listener),
    ])


def set_listener(u):
    tag = 'set-listener'
    u.set_listener(DEVICE_ID, '192.168.1.100:60001')

    return evaluate(tag, [])


def get_door_control(u):
    tag = 'get-door-control'
    control = u.get_door_control(DEVICE_ID, DOOR)

    return evaluate(tag, [
        ('door control mode', CONTROLLED, control.mode),
        ('door open delay', 7, control.delay),
    ])


def set_door_control(u):
    tag = 'set-door-control'
    u.set_door_control(DEVICE_ID, DOOR, NORMALLY_CLOSED, 6)

    return evaluate(tag, [])


def open_door(u):
    tag = 'open-door'
    u.open_door(DEVICE_ID, DOOR)

    return evaluate(tag, [])


def get_cards(u):
    tag = 'get-cards'
    cards = u.get_cards(DEVICE_ID)

    return evaluate(tag, [
        ('card count', 39, cards),
    ])


def get_card(u):
    tag = 'get-card'
    card = u.get_card(DEVICE_ID, CARD_NUMBER)

    return evaluate(tag, [
        ('card number', 8165538, card.cardNumber),
        ('from date', '2022-01-01', card.start),
        ('to date', '2022-12-31', card.end),
        ('door[1]', 0, card.doors[0]),
        ('door[2]', 1, card.doors[1]),
        ('door[3]', 31, card.doors[2]),
        ('door[42]', 75, card.doors[3]),
    ])


def get_card_by_index(u):
    tag = 'get-card-by-index'
    card = u.get_card_by_index(DEVICE_ID, CARD_INDEX)

    return evaluate(tag, [
        ('card number', 8165538, card.cardNumber),
        ('from date', '2022-01-01', card.start),
        ('to date', '2022-12-31', card.end),
        ('door[1]', 0, card.doors[0]),
        ('door[2]', 1, card.doors[1]),
        ('door[3]', 31, card.doors[2]),
        ('door[42]', 75, card.doors[3]),
    ])


def put_card(u):
    tag = 'put-card'
    u.put_card(DEVICE_ID, CARD_NUMBER, '2022-01-01', '2022-12-31', [0, 1, 31, 75])

    return evaluate(tag, [])


def delete_card(u):
    tag = 'delete-card'
    u.delete_card(DEVICE_ID, CARD_NUMBER)

    return evaluate(tag, [])


def delete_cards(u):
    tag = 'delete-cards'
    u.delete_cards(DEVICE_ID)

    return evaluate(tag, [])


def get_event_index(u):
    tag = 'get-event-index'
    index = u.get_event_index(DEVICE_ID)

    return evaluate(tag, [
        ('event index', 47, index),
    ])


def set_event_index(u):
    tag = 'set-event-index'
    u.set_event_index(DEVICE_ID, EVENT_INDEX)

    return evaluate(tag, [])


def get_event(u):
    tag = 'get-event'
    event = u.get_event(DEVICE_ID, EVENT_INDEX)

    return evaluate(tag, [
        ('event index', 51, event.index),
        ('event timestamp', '2022-04-15 12:29:15', event.timestamp),
        ('event type', 6, event.eventType),
        ('event granted', True, event.granted),
        ('event door', 3, event.door),
        ('event direction', 1, event.direction),
        ('event card', 8165538, event.card),
        ('event reason', 21, event.reason),
    ])


def record_special_events(u):
    tag = 'record-special-events'
    u.record_special_events(DEVICE_ID, True)

    return evaluate(tag, [])


def evaluate(tag, resultset):
    ok = True
    for row in resultset:
        field, expected, actual = row
        if actual != expected:
            print(f'{tag:<17} incorrect {field} (expected:{expected}, got:{actual})')
            ok = False

    return passed(tag) if ok else failed(tag)


def passed(tag):
    print(f'{tag:<21} ok')
    return True


def failed(tag):
    print(f'{tag:<21} failed')
    return False


def usage():
    print('   Usage: python test.py <command>')
    print()
    print('   Supported commands:')
    print('      all')
    for k in tests():
        print(f'      {k}')
    print()


def main():
    cmd = ''

    if len(sys.argv) > 1:
        cmd = sys.argv[1]

    bind = '192.168.1.100'
    broadcast = '192.168.1.255'
    listen = '192.168.1.100:60001'
    timeout = 2500
    debug = True

    controllers = [
        uhppoted.Controller(405419896, '192.168.1.100'),
        uhppoted.Controller(303986753, '192.168.1.100'),
    ]

    u = uhppoted.Uhppote(uhppote=uhppoted.UHPPOTE(bind, broadcast, listen, timeout, controllers, debug))

    try:
        if cmd in tests():
            if not tests()[cmd](u):
                sys.exit(-1)

        elif cmd == '' or cmd == 'all':
            if not reduce(lambda ok, f: f(u) and ok, tests().values(), True):
                sys.exit(-1)

        elif cmd == 'help':
            print()
            usage()

        else:
            print()
            print(f'   *** ERROR invalid command ({cmd})')
            print()
            usage()

    except BaseException as x:
        print()
        print(f'*** ERROR  {cmd} failed: {x}')
        print()

        sys.exit(1)


if __name__ == '__main__':
    main()
