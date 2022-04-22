#!python

import argparse
import ctypes
import sys
import datetime

sys.path.append('../../bindings/python')

import uhppoted

from uhppoted import NORMALLY_OPEN
from uhppoted import NORMALLY_CLOSED
from uhppoted import CONTROLLED

DEVICE_ID = 405419896
CARD_NUMBER = 8000001
CARD_INDEX = 7
EVENT_INDEX = 43
DOOR = 4
PROFILE_ID = 29


def commands():
    return {
        'get-devices': {
            'help': "Retrieves a list of UHPPOTE controller IDs findable on the local LAN.",
            'fn': get_devices,
        },
        'get-device': {
            'help': "Retrieves the basic device information for a single UHPPOTE controller.",
            'fn': get_device,
        },
        'set-address': {
            'help': "Sets the controller IPv4 address, subnet mask and gateway address.",
            'fn': set_address,
        },
        'get-status': {
            'help': "Retrieves a controller status.",
            'fn': get_status,
        },
        'get-time': {
            'help': "Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss).",
            'fn': get_time,
        },
        'set-time': {
            'help': "Sets a controller current date/time (YYYY-MM-DD HH:mm:ss).",
            'fn': set_time,
        },
        'get-listener': {
            'help': "Retrieves a controller's configured event listener address.",
            'fn': get_listener,
        },
        'set-listener': {
            'help': "Configures a controller's event listener address and port.",
            'fn': set_listener,
        },
        'get-door-control': {
            'help': "Retrieves the control state and open delay for a controller door.",
            'fn': get_door_control,
        },
        'set-door-control': {
            'help': "Sets the control mode and delay for a controller door.",
            'fn': set_door_control,
        },
        'open-door': {
            'help': "Remotely opens a controller door.",
            'fn': open_door,
        },
        'get-cards': {
            'help': "Retrieves the number of cards stored on a controller.",
            'fn': get_cards,
        },
        'get-card': {
            'help': "Retrieves the card detail for card number from a controller.",
            'fn': get_card,
        },
        'get-card-by-index': {
            'help': "Retrieves the card detail for the card stored at an index on a controller.",
            'fn': get_card_by_index,
        },
        'put-card': {
            'help': "Adds or updates the card detail for card number stored on a controller.",
            'fn': put_card,
        },
        'delete-card': {
            'help': "Deletes a card from a controller.",
            'fn': delete_card,
        },
        'delete-cards': {
            'help': "Deletes all cards from a controller.",
            'fn': delete_cards,
        },
        'get-event-index': {
            'help': "Retrieves the current event index from a controller.",
            'fn': get_event_index,
        },
        'set-event-index': {
            'help': "Sets the current event index on a controller.",
            'fn': set_event_index,
        },
        'get-event': {
            'help': "Retrieves the event at the index from a controller.",
            'fn': get_event,
        },
        'record-special-events': {
            'help': "Enables/disables recording additional events for a controller.",
            'fn': record_special_events,
        },
        'get-time-profile': {
            'help': "Retrieves a time profile from a controller.",
            'fn': get_time_profile,
        },
        'set-time-profile': {
            'help': "Adds or updates a time profile on a controller.",
            'fn': set_time_profile,
        },
    }


def usage():
    print()
    print('  Usage: python example.py <command>')
    print()
    print('  Supported commands:')

    for cmd, _ in commands().items():
        print(f'    {cmd}')

    print()


def help():
    print()
    print('Usage: python example.py <command>')
    print()
    print('  Commands')

    for cmd, v in commands().items():
        print(f"    {cmd:<17}  {v['help']}")

    print()


def get_devices(u, args):
    try:
        list = u.get_devices()

        print(f'get-devices ({len(list)})')
        for id in list:
            print(f'  {id}')
        print()

    except Exception as e:
        print(f' *** ERROR get-devices ({e})')
        print()


def get_device(u, args):
    deviceID = DEVICE_ID

    try:
        info = u.get_device(deviceID)

        print('get-device')
        print(f'  ID:       {info.ID}')
        print(f'  IP:       {info.address}  {info.subnet}  {info.gateway}')
        print(f'  MAC:      {info.MAC}')
        print(f'  version:  {info.version}')
        print(f'  released: {info.date}')
        print()

    except Exception as e:
        print(f' *** ERROR get-device ({e})')
        print()


def set_address(u, args):
    deviceID = DEVICE_ID
    address = '192.168.1.125'
    subnet = '255.255.255.253'
    gateway = '192.168.1.5'

    try:
        u.set_address(deviceID, address, subnet, gateway)

        print('set-address')
        print(f'  ID:      {deviceID}')
        print(f'  address: {address}')
        print(f'  subnet:  {subnet}')
        print(f'  gateway: {gateway}')
        print()

    except Exception as e:
        print(f' *** ERROR set-address ({e})')
        print()


def get_status(u, args):
    deviceID = DEVICE_ID

    try:
        status = u.get_status(deviceID)

        print('get-status')
        print(f'  ID:        {status.ID}')
        print(f'  date/time: {status.sysdatetime}')
        print(
            f'  doors:     {status.doors[0]} {status.doors[1]} {status.doors[2]} {status.doors[3]}')
        print(
            f'  buttons:   {status.buttons[0]} {status.buttons[1]} {status.buttons[2]} {status.buttons[3]}'
        )
        print(f'  relays   : ' + '{0:#0{1}x}'.format(status.relays, 4))
        print(f'  inputs   : ' + '{0:#0{1}x}'.format(status.inputs, 4))
        print(f'  error    : ' + '{0:#0{1}x}'.format(status.syserror, 4))
        print(f'  info     : ' + '{0:#0{1}x}'.format(status.info, 4))
        print(f'  seq no.  : {status.seqno}')
        print(f'  event timestamp: {status.event.timestamp}')
        print(f'        index:     {status.event.index}')
        print(f'        type:      {status.event.eventType}')
        print(f'        granted:   {status.event.granted}')
        print(f'        door:      {status.event.door}')
        print(f'        direction: {status.event.direction}')
        print(f'        card:      {status.event.card}')
        print(f'        reason:    {status.event.reason}')
        print()

    except Exception as e:
        print(f' *** ERROR get-status ({e})')
        print()


def get_time(u, args):
    deviceID = DEVICE_ID

    try:
        datetime = u.get_time(deviceID)

        print('get-time')
        print(f'  date/time: {datetime}')
        print()

    except Exception as e:
        print(f' *** ERROR get-time ({e})')
        print()


def set_time(u, args):
    deviceID = DEVICE_ID
    dt = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    try:
        u.set_time(deviceID, dt)

        print('set-time')
        print(f'  ID:        {deviceID}')
        print(f'  date/time: {dt}')
        print()

    except Exception as e:
        print(f' *** ERROR set-time ({e})')
        print()


def get_listener(u, args):
    deviceID = DEVICE_ID

    try:
        listener = u.get_listener(deviceID)

        print('get-listener')
        print(f'  listener: {listener}')
        print()

    except Exception as e:
        print(f' *** ERROR get-listener ({e})')
        print()


def set_listener(u, args):
    deviceID = DEVICE_ID
    listener = '192.168.1.100:60001'

    try:
        u.set_listener(deviceID, listener)

        print('set-listener')
        print(f'  ID:             {deviceID}')
        print(f'  event listener: {listener}')
        print()

    except Exception as e:
        print(f' *** ERROR set-time ({e})')
        print()


def get_door_control(u, args):
    deviceID = DEVICE_ID
    door = DOOR

    try:
        control = u.get_door_control(deviceID, door)

        print('get-door-control')
        print(f'  ID:    {deviceID}')
        print(f'  door:  {door}')

        if control.mode == NORMALLY_OPEN:
            print(f'  mode:  normally open')
        elif control.mode == NORMALLY_CLOSED:
            print(f'  mode:  normally closed')
        elif control.mode == CONTROLLED:
            print(f'  mode:  controlled')
        else:
            print(f'  mode:  ???')

        print(f'  delay: {control.delay}')
        print()

    except Exception as e:
        print(f' *** ERROR get-door-control ({e})')
        print()


def set_door_control(u, args):
    deviceID = DEVICE_ID
    door = DOOR
    mode = NORMALLY_OPEN
    delay = 9

    try:
        u.set_door_control(deviceID, door, mode, delay)

        print('set-door-control')
        print(f'  ID:    {deviceID}')
        print(f'  door:  {door}')

        if mode == NORMALLY_OPEN:
            print(f'  mode:  normally open')
        elif mode == NORMALLY_CLOSED:
            print(f'  mode:  normally closed')
        elif mode == CONTROLLED:
            print(f'  mode:  controlled')
        else:
            print(f'  mode:  ???')

        print(f'  delay: {delay}')
        print()

    except Exception as e:
        print(f' *** ERROR set-door-control ({e})')
        print()


def open_door(u, args):
    tag = 'open-door'
    deviceID = DEVICE_ID
    door = DOOR

    try:
        u.open_door(deviceID, door)

        print(f'{tag}')
        print(f'  ID:    {deviceID}')
        print(f'  door:  {door}')
        print()

    except Exception as e:
        print(f' *** ERROR {tag} ({e})')
        print()


def get_cards(u, args):
    deviceID = DEVICE_ID

    try:
        cards = u.get_cards(deviceID)

        print('get-cards')
        print(f'  ID:    {deviceID}')
        print(f'  cards: {cards}')
        print()

    except Exception as e:
        print(f' *** ERROR get_cards ({e})')
        print()


def get_card(u, args):
    deviceID = DEVICE_ID
    cardNumber = CARD_NUMBER

    try:
        card = u.get_card(deviceID, cardNumber)

        print('get-card')
        print(f'  ID:           {deviceID}')
        print(f'  card-number:  {card.cardNumber}')
        print(f'       from:    {card.start}')
        print(f'       to:      {card.end}')
        print(f'       door[1]: {card.doors[0]}')
        print(f'       door[2]: {card.doors[1]}')
        print(f'       door[3]: {card.doors[2]}')
        print(f'       door[4]: {card.doors[3]}')
        print()

    except Exception as e:
        print(f' *** ERROR get_card ({e})')
        print()


def get_card_by_index(u, args):
    deviceID = DEVICE_ID
    index = CARD_INDEX

    try:
        card = u.get_card_by_index(deviceID, index)

        print('get-card-by-index')
        print(f'  ID:           {deviceID}')
        print(f'  index:        {index}')
        print(f'  card-number:  {card.cardNumber}')
        print(f'       from:    {card.start}')
        print(f'       to:      {card.end}')
        print(f'       door[1]: {card.doors[0]}')
        print(f'       door[2]: {card.doors[1]}')
        print(f'       door[3]: {card.doors[2]}')
        print(f'       door[4]: {card.doors[3]}')
        print()

    except Exception as e:
        print(f' *** ERROR get_card ({e})')
        print()


def put_card(u, args):
    try:
        deviceID = DEVICE_ID
        cardNumber = CARD_NUMBER
        start = '2022-01-01'
        end = '2022-12-31'
        doors = [0, 1, 31, 75]

        card = u.put_card(deviceID, cardNumber, start, end, doors)

        print('put-card')
        print(f'  ID:           {deviceID}')
        print(f'  card-number:  {cardNumber}')
        print(f'       from:    {start}')
        print(f'       to:      {end}')
        print(f'       door[1]: {doors[0]}')
        print(f'       door[2]: {doors[1]}')
        print(f'       door[3]: {doors[2]}')
        print(f'       door[4]: {doors[3]}')
        print()

    except Exception as e:
        print(f' *** ERROR put_card ({e})')
        print()


def delete_card(u, args):
    try:
        deviceID = DEVICE_ID
        cardNumber = CARD_NUMBER

        u.delete_card(deviceID, cardNumber)

        print('delete-card')
        print(f'  ID:           {deviceID}')
        print(f'  card-number:  {cardNumber}')
        print()

    except Exception as e:
        print(f' *** ERROR delete_card ({e})')
        print()


def delete_cards(u, args):
    try:
        deviceID = DEVICE_ID

        u.delete_cards(deviceID)

        print('delete-cards')
        print(f'  ID: {deviceID}')
        print()

    except Exception as e:
        print(f' *** ERROR delete_cards ({e})')
        print()


def get_event_index(u, args):
    tag = 'get-event-index'
    deviceID = DEVICE_ID

    try:
        index = u.get_event_index(deviceID)

        print(f'{tag}')
        print(f'  ID:    {deviceID}')
        print(f'  index: {index}')
        print()

    except Exception as e:
        print(f' *** ERROR {tag} ({e})')
        print()


def set_event_index(u, args):
    tag = 'set-event-index'
    deviceID = DEVICE_ID
    index = EVENT_INDEX

    try:
        u.set_event_index(deviceID, index)

        print(f'{tag}')
        print(f'  ID:    {deviceID}')
        print(f'  index: {index}')
        print()

    except Exception as e:
        print(f' *** ERROR {tag} ({e})')
        print()


def get_event(u, args):
    tag = 'get-event'
    deviceID = DEVICE_ID
    index = EVENT_INDEX

    try:
        event = u.get_event(deviceID, index)

        print(f'{tag}')
        print(f'  ID:                {deviceID}')
        print(f'  event index:       {event.index}')
        print(f'        timestamp:   {event.timestamp}')
        print(f'        type:        {event.eventType}')
        print(f'        granted:     {event.granted}')
        print(f'        door:        {event.door}')
        print(f'        direction:   {event.direction}')
        print(f'        card number: {event.card}')
        print(f'        reason:      {event.reason}')
        print()

    except Exception as e:
        print(f' *** ERROR {tag} ({e})')
        print()


def record_special_events(u, args):
    tag = 'record-special-events'
    deviceID = DEVICE_ID
    enabled = True

    try:
        u.record_special_events(deviceID, enabled)

        print(f'{tag}')
        print(f'  ID:      {deviceID}')
        print(f'  enabled: {enabled}')
        print()

    except Exception as e:
        print(f' *** ERROR {tag} ({e})')
        print()


def get_time_profile(u, args):
    tag = 'get-time-profile'
    deviceID = DEVICE_ID
    profileID = PROFILE_ID

    try:
        profile = u.get_time_profile(deviceID, profileID)

        print(f'{tag}')
        print(f'  ID:                   {deviceID}')
        print(f'  profile ID:           {profile.ID}')
        print(f'  linked profile:       {profile.linked}')
        print(f'  enabled from:         {profile.start}')
        print(f'          to:           {profile.end}')
        print(f'  enabled on Monday:    {profile.monday}')
        print(f'             Tuesday:   {profile.tuesday}')
        print(f'             Wednesday: {profile.wednesday}')
        print(f'             Thursday:  {profile.thursday}')
        print(f'             Friday:    {profile.friday}')
        print(f'             Saturday:  {profile.saturday}')
        print(f'             Sunday:    {profile.sunday}')
        print(f'  segment 1:            {profile.segment1start}-{profile.segment1end}')
        print(f'  segment 2:            {profile.segment2start}-{profile.segment2end}')
        print(f'  segment 3:            {profile.segment3start}-{profile.segment3end}')
        print()

    except Exception as e:
        print(f' *** ERROR {tag} ({e})')
        print()


def set_time_profile(u, args):
    tag = 'set-time-profile'
    deviceID = DEVICE_ID

    try:
        profile = uhppoted.TimeProfile(PROFILE_ID, 71, "2022-02-01", "2022-06-30", True, False,
                                       True, True, False, False, True, "08:30", "11:30", "", "", "",
                                       "18:00")

        u.set_time_profile(deviceID, profile)

        print(f'{tag}')
        print(f'  ID:                   {deviceID}')
        print(f'  profile ID:           {profile.ID}')
        print(f'  linked profile:       {profile.linked}')
        print(f'  enabled from:         {profile.start}')
        print(f'          to:           {profile.end}')
        print(f'  enabled on Monday:    {profile.monday}')
        print(f'             Tuesday:   {profile.tuesday}')
        print(f'             Wednesday: {profile.wednesday}')
        print(f'             Thursday:  {profile.thursday}')
        print(f'             Friday:    {profile.friday}')
        print(f'             Saturday:  {profile.saturday}')
        print(f'             Sunday:    {profile.sunday}')
        print(f'  segment 1:            {profile.segment1start}-{profile.segment1end}')
        print(f'  segment 2:            {profile.segment2start}-{profile.segment2end}')
        print(f'  segment 3:            {profile.segment3start}-{profile.segment3end}')
        print()

    except Exception as e:
        print(f' *** ERROR {tag} ({e})')
        print()


def main():
    if len(sys.argv) < 2:
        usage()
        return -1

    cmd = sys.argv[1]

    if cmd == 'help':
        help()
    else:
        bind = '192.168.1.100'
        broadcast = '192.168.1.255'
        listen = '192.168.1.100:60001'
        timeout = 2500
        debug = True
        controllers = [
            uhppoted.Controller(405419896, '192.168.1.100'),
            uhppoted.Controller(303986753, '192.168.1.100'),
        ]

        u = uhppoted.Uhppote(
            uhppote=uhppoted.UHPPOTE(bind, broadcast, listen, timeout, controllers, debug))

        try:
            for c, v in commands().items():
                if cmd == c:
                    v['fn'](u, None)
                    return

            print()
            print(f'  ERROR: invalid command ({cmd})')
            usage()

        except BaseException as x:
            print()
            print(f'*** ERROR  {cmd} failed: {x}')
            print()

            sys.exit(1)


if __name__ == '__main__':
    main()
