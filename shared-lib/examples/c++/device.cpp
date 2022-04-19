#include <iostream>

#include "../include/uhppoted.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;
extern const uint8_t DOOR;

int getDevices(uhppoted &u, int argc, char **argv) {
    try {
        auto devices = u.get_devices();

        cout << endl
             << "get-devices (" << devices.size() << ")" << endl;
        for (auto id : devices) {
            cout << "   " << id << endl;
        }
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int getDevice(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    try {
        auto d = u.get_device(deviceID);

        cout << endl
             << "get-device" << endl;
        cout << "  ID:       " << d.ID << endl;
        cout << "  IP:       " << d.address << "  " << d.subnet << "  " << d.gateway
             << endl;
        cout << "  MAC:      " << d.MAC << endl;
        cout << "  version:  " << d.version << endl;
        cout << "  released: " << d.date << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int setAddress(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    string address = "192.168.1.125";
    string subnet = "255.255.254.0";
    string gateway = "192.168.1.10";

    try {
        u.set_address(deviceID, address, subnet, gateway);

        cout << endl
             << "set-address" << endl;
        cout << "  ID:       " << deviceID << endl;
        cout << "  address:  " << address << endl;
        cout << "  subnet:   " << subnet << endl;
        cout << "  gateway:  " << gateway << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int getStatus(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    try {
        auto s = u.get_status(deviceID);

        cout << endl
             << "get-status" << endl;
        cout << "  ID:        " << s.ID << endl;

        cout << "  date/time: " << s.sysdatetime << endl;
        cout << "  doors:     " << static_cast<int>(s.doors[0]) << " "
             << static_cast<int>(s.doors[1]) << " " << static_cast<int>(s.doors[2])
             << " " << static_cast<int>(s.doors[3]) << endl;
        cout << "  buttons:   " << static_cast<int>(s.buttons[0]) << " "
             << static_cast<int>(s.buttons[1]) << " "
             << static_cast<int>(s.buttons[2]) << " "
             << static_cast<int>(s.buttons[3]) << endl;
        cout << "  relays:    " << showbase << hex << static_cast<int>(s.relays)
             << dec << endl;
        cout << "  inputs:    " << showbase << hex << static_cast<int>(s.inputs)
             << dec << endl;
        cout << "  error:     " << showbase << hex << static_cast<int>(s.syserror)
             << dec << endl;
        cout << "  seq no.:   " << s.seqno << endl;
        cout << "  info:      " << showbase << hex << static_cast<int>(s.info)
             << dec << endl;
        cout << endl;
        cout << "  event timestamp: " << s.event.timestamp << endl;
        cout << "        index:     " << s.event.index << endl;
        cout << "        type:      " << static_cast<int>(s.event.eventType)
             << endl;
        cout << "        granted:   " << s.event.granted << endl;
        cout << "        door:      " << static_cast<int>(s.event.door) << endl;
        cout << "        direction: " << static_cast<int>(s.event.direction)
             << endl;
        cout << "        card:      " << s.event.card << endl;
        cout << "        reason:    " << static_cast<int>(s.event.reason) << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int getTime(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    try {
        auto datetime = u.get_time(deviceID);

        cout << endl
             << "get-time" << endl;
        cout << "  date/time: " << datetime << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int setTime(uhppoted &u, int argc, char **argv) {
    char s[20];
    time_t now = time(nullptr);

    strftime(s, sizeof(s), "%Y-%m-%d %H:%M:%S", localtime(&now));

    uint32_t deviceID = DEVICE_ID;
    string datetime = s;

    try {
        u.set_time(deviceID, datetime);

        cout << endl
             << "set-time" << endl;
        cout << "  ID:       " << deviceID << endl;
        cout << "  datetime: " << datetime << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int getListener(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    try {
        auto listener = u.get_listener(deviceID);

        cout << endl
             << "get-listener" << endl;
        cout << "  listener: " << listener << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int setListener(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    string listener = "192.168.1.100:60001";

    try {
        u.set_listener(deviceID, listener);

        cout << endl
             << "set-listener" << endl;
        cout << "  ID:             " << deviceID << endl;
        cout << "  event listener: " << listener << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int getDoorControl(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t door = DOOR;

    try {
        auto d = u.get_door_control(deviceID, door);

        cout << endl
             << "get-door-control" << endl;
        cout << "  ID:      " << deviceID << endl;
        cout << "  door:    " << static_cast<int>(door) << endl;

        switch (d.mode) {
        case NORMALLY_OPEN:
            cout << "  mode:    "
                 << "normally open" << endl;
            break;

        case NORMALLY_CLOSED:
            cout << "  mode:    "
                 << "normally closed" << endl;
            break;

        case CONTROLLED:
            cout << "  mode:    "
                 << "controlled" << endl;
            break;

        default:
            cout << "  mode:    "
                 << "???" << endl;
            break;
        }

        cout << "  delay:   " << static_cast<int>(d.delay) << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int setDoorControl(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t door = DOOR;
    uint8_t mode = NORMALLY_OPEN;
    uint8_t delay = 9;

    try {
        u.set_door_control(deviceID, door, mode, delay);

        cout << endl
             << "set-door-control" << endl;
        cout << "  ID:      " << deviceID << endl;
        cout << "  door:    " << static_cast<int>(door) << endl;

        switch (mode) {
        case NORMALLY_OPEN:
            cout << "  mode:    "
                 << "normally open" << endl;
            break;

        case NORMALLY_CLOSED:
            cout << "  mode:    "
                 << "normally closed" << endl;
            break;

        case CONTROLLED:
            cout << "  mode:    "
                 << "controlled" << endl;
            break;

        default:
            cout << "  mode:    "
                 << "???" << endl;
            break;
        }

        cout << "  delay:   " << static_cast<int>(delay) << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int openDoor(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t door = DOOR;

    try {
        u.open_door(deviceID, door);

        cout << endl
             << "open-door" << endl;
        cout << "  ID:      " << deviceID << endl;
        cout << "  door:    " << static_cast<int>(door) << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}
