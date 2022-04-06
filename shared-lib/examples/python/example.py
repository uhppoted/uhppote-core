#!python

import argparse
import ctypes
import sys
import datetime

sys.path.append('../../bindings/python')

import uhppoted


def commands():
    return {
        'get-devices': None,
        'get-device': None,
        'set-address': None,
        'get-status': None,
        'get-time': None,
        'set-time': None,
        'get-listener': None,
        'set-listener': None,
        'get-door-control': None,
        'set-door-control': None,
        'get-cards': None,
        'get-card': None,
    }


def usage():
    print()
    print("  Usage: python example.py <command>")
    print()
    print("  Supported commands:")

    for cmd, _ in commands().items():
        print(f"    {cmd}")

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
    print("    get-time")
    print("    set-time")
    print("    get-listener")
    print("    set-listener")
    print("    get-door-control")
    print("    set-door-control")
    print("    get-cards")
    print("    help")
    print()
    print("  get-devices")
    print("    Retrieves a list of UHPPOTE controller IDs findable on the local LAN.")
    print()
    print("  get-device")
    print("    Retrieves the basic device information for a single UHPPOTE controller.")
    print()
    print("  set-address")
    print("    Sets the controller IPv4 address, subnet mask and gateway address.")
    print()
    print("  get-status")
    print("    Retrieves a controller status.")
    print()
    print("  get-time")
    print("    Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss).")
    print()
    print("  set-time")
    print("    Sets a controller current date/time (YYYY-MM-DD HH:mm:ss).")
    print()
    print("  get-listener")
    print("    Retrieves a controller's configured event listener address.")
    print()
    print("  set-listener")
    print("    Configures a controller's event listener address and port.")
    print()
    print("  get-door-control")
    print("    Retrieves the control state and open delay for a controller door.")
    print()
    print("  set-door-control")
    print("    Sets the control mode and delay for a controller door.")
    print()
    print("  get-cards")
    print("    Retrieves the number of cards stored on a controller.")
    print()
    print("  get-card")
    print("    Retrieves the card detail for card number from a controller.")
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
            f"  doors:     {status.doors[0]} {status.doors[1]} {status.doors[2]} {status.doors[3]}")
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


def get_time(u, deviceID):
    try:
        datetime = u.get_time(deviceID)

        print("get-time")
        print(f"  date/time: {datetime}")
        print()

    except Exception as e:
        print(f" *** ERROR get-time ({e})")
        print()


def set_time(u, deviceID, datetime):
    try:
        u.set_time(deviceID, datetime)

        print("set-time")
        print(f"  ID:        {deviceID}")
        print(f"  date/time: {datetime}")
        print()

    except Exception as e:
        print(f" *** ERROR set-time ({e})")
        print()


def get_listener(u, deviceID):
    try:
        listener = u.get_listener(deviceID)

        print("get-listener")
        print(f"  listener: {listener}")
        print()

    except Exception as e:
        print(f" *** ERROR get-listener ({e})")
        print()


def set_listener(u, deviceID, listener):
    try:
        u.set_listener(deviceID, listener)

        print("set-listener")
        print(f"  ID:             {deviceID}")
        print(f"  event listener: {listener}")
        print()

    except Exception as e:
        print(f" *** ERROR set-time ({e})")
        print()


def get_door_control(u, deviceID, door):
    try:
        control = u.get_door_control(deviceID, door)

        print("get-door-control")
        print(f"  ID:    {deviceID}")
        print(f"  door:  {door}")

        if control.mode == 1:
            print(f"  mode:  normally open")
        elif control.mode == 2:
            print(f"  mode:  normally closed")
        elif control.mode == 3:
            print(f"  mode:  controlled")
        else:
            print(f"  mode:  ???")

        print(f"  delay: {control.delay}")
        print()

    except Exception as e:
        print(f" *** ERROR get-door-control ({e})")
        print()


def set_door_control(u, deviceID, door, mode, delay):
    try:
        u.set_door_control(deviceID, door, mode, delay)

        print("set-door-control")
        print(f"  ID:    {deviceID}")
        print(f"  door:  {door}")

        if mode == 1:
            print(f"  mode:  normally open")
        elif mode == 2:
            print(f"  mode:  normally closed")
        elif mode == 3:
            print(f"  mode:  controlled")
        else:
            print(f"  mode:  ???")

        print(f"  delay: {delay}")
        print()

    except Exception as e:
        print(f" *** ERROR set-door-control ({e})")
        print()


def get_cards(u, deviceID):
    try:
        cards = u.get_cards(deviceID)

        print("get-cards")
        print(f"  ID:    {deviceID}")
        print(f"  cards: {cards}")
        print()

    except Exception as e:
        print(f" *** ERROR get_cards ({e})")
        print()


def get_card(u, deviceID, cardNumber):
    try:
        card = u.get_card(deviceID, cardNumber)

        print("get-card")
        print(f"  ID:           {deviceID}")
        print(f"  card-number:  {card.cardNumber}")
        print(f"       from:    {card.start}")
        print(f"       to:      {card.end}")
        print(f"       door[1]: {card.doors[0]}")
        print(f"       door[2]: {card.doors[1]}")
        print(f"       door[3]: {card.doors[2]}")
        print(f"       door[4]: {card.doors[3]}")
        print()

    except Exception as e:
        print(f" *** ERROR get_card ({e})")
        print()


def main():
    if len(sys.argv) < 2:
        usage()
        return -1

    cmd = sys.argv[1]

    if cmd == "help":
        help()

    else:
        alpha = uhppoted.Controller(405419896, "192.168.1.100")
        beta = uhppoted.Controller(303986753, "192.168.1.100")
        controllers = [alpha, beta]

        u = uhppoted.Uhppote(uhppote=uhppoted.UHPPOTE(
            '192.168.1.100', '192.168.1.255', '192.168.1.100:60001', 2500, controllers, True))
        try:
            if cmd == 'get-devices':
                get_devices(u)

            elif cmd == 'get-device':
                get_device(u, 405419896)

            elif cmd == 'set-address':
                set_address(u, 405419896, "192.168.1.125", "255.255.255.253", "192.168.1.5")

            elif cmd == 'get-status':
                get_status(u, 405419896)

            elif cmd == 'get-time':
                get_time(u, 405419896)

            elif cmd == 'set-time':
                set_time(u, 405419896, datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S'))

            elif cmd == 'get-listener':
                get_listener(u, 405419896)

            elif cmd == 'set-listener':
                set_listener(u, 405419896, "192.168.1.100:60001")

            elif cmd == 'get-door-control':
                get_door_control(u, 405419896, 4)

            elif cmd == 'set-door-control':
                set_door_control(u, 405419896, 4, 1, 9)

            elif cmd == 'get-cards':
                get_cards(u, 405419896)

            elif cmd == 'get-card':
                get_card(u, 405419896, 8000001)

            else:
                print()
                print(f"  ERROR: invalid command ({cmd})")
                usage()

        except BaseException as x:
            print()
            print(f"*** ERROR  {cmd} failed: {x}")
            print()

            sys.exit(1)


if __name__ == "__main__":
    main()
