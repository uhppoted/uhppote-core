#include <iostream>

#include "device.hpp"

using namespace std;

int getDevices(uhppoted &u) {
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

int getDevice(uhppoted &u, uint32_t deviceID) {
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

int setAddress(uhppoted &u, uint32_t deviceID, string address, string subnet,
               string gateway) {
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

int getStatus(uhppoted &u, uint32_t deviceID) {
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

int getTime(uhppoted &u, uint32_t deviceID) {
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

int setTime(uhppoted &u, uint32_t deviceID, string datetime) {
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

int getListener(uhppoted &u, uint32_t deviceID) {
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
