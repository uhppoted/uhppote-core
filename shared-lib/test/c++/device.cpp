#include <iostream>

#include "device.hpp"

using namespace std;

bool getDevices(uhppoted &u) {
    try {
        auto devices = u.get_devices();
        bool ok = true;

        if (devices.size() != 3) {
            cout << "get-devices: incorrect device count - expected:" << 3 << ", got:" << devices.size() << endl;
            ok = false;
        } else if (devices[0] != 201020304 || devices[1] != 303986753 || devices[2] != 405419896) {
            cout << "get-devices: incorrect device list - expected:[" << 201020304 << "," << 303986753 << "," << 405419896 << "]"
                 << ", got:[" << devices[0] << "," << devices[1] << "," << devices[2] << "]" << endl;

            ok = false;
        }

        if (ok) {
            cout << "get-devices: ok" << endl;
        }

        return ok;

    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool getDevice(uhppoted &u, uint32_t deviceID) {
    try {
        auto d = u.get_device(deviceID);
        bool ok = true;

        if (d.ID != 405419896) {
            cout << "get-device: incorrect device ID - expected:" << 405419896 << ", got:" << d.ID << endl;
            ok = false;
        }

        if (d.address != "192.168.1.101") {
            cout << "get-device: incorrect IP address - expected:"
                 << "192.168.1.101"
                 << ", got:" << d.address << endl;
            ok = false;
        }

        if (d.subnet != "255.255.255.0") {
            cout << "get-device: incorrect subnet mask - expected:"
                 << "255.255.255.0"
                 << ", got:" << d.subnet << endl;
            ok = false;
        }

        if (d.gateway != "192.168.1.1") {
            cout << "get-device: incorrect gateway address - expected:"
                 << "192.168.1.1"
                 << ", got:" << d.gateway << endl;
            ok = false;
        }

        if (d.MAC != "00:12:23:34:45:56") {
            cout << "get-device: incorrect MAC address - expected:"
                 << "00:12:23:34:45:56"
                 << ", got:" << d.MAC << endl;
            ok = false;
        }

        if (d.version != "v8.92") {
            cout << "get-device: incorrect version - expected:"
                 << "v8.92"
                 << ", got:" << d.version << endl;
            ok = false;
        }

        if (d.date != "2018-11-05") {
            cout << "get-device: incorrect date - expected:"
                 << "2018-11-05"
                 << ", got:" << d.date << endl;
            ok = false;
        }

        if (ok) {
            cout << "get-device:  ok" << endl;
        }

        return ok;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool setAddress(uhppoted &u, uint32_t deviceID, string address, string subnet, string gateway) {
    try {
        u.set_address(deviceID, address, subnet, gateway);

        cout << "set-address: ok" << endl;

        return true;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool getStatus(uhppoted &u, uint32_t deviceID) {
    try {
        auto s = u.get_status(deviceID);
        bool ok = true;

        if (s.ID != 405419896) {
            cout << "get-status: incorrect device ID - expected:" << 405419896 << ", got:" << s.ID << endl;
            ok = false;
        }

        if (string(s.sysdatetime) != "2022-03-19 15:48:32") {
            cout << "get-status: incorrect system date/time - expected:"
                 << "2022-03-19 15:48:32"
                 << ", got:" << s.sysdatetime << endl;
            ok = false;
        }

        if (s.doors[0] != 1 || s.doors[1] != 0 || s.doors[2] != 0 || s.doors[3] != 1) {
            cout << "get-status: incorrect doors state - "
                 << "expected:[" << 1 << "," << 0 << "," << 0 << "," << 1 << "], "
                 << "got:[" << s.doors[0] << "," << s.doors[1] << "," << s.doors[2] << "," << s.doors[3] << "]"
                 << endl;
            ok = false;
        }

        if (s.buttons[0] != 1 || s.buttons[1] != 0 || s.buttons[2] != 1 || s.buttons[3] != 0) {
            cout << "get-status: incorrect buttons state - "
                 << "expected:[" << 1 << "," << 0 << "," << 0 << "," << 1 << "], "
                 << "got:[" << s.buttons[0] << ", " << s.buttons[1] << ", " << s.buttons[2] << ", " << s.buttons[3] << "]"
                 << endl;
            ok = false;
        }

        if (s.relays != 0x12) {
            cout << "get-status: incorrect relay state - expected:" << 0x12 << ", got:" << s.relays << endl;
            ok = false;
        }

        if (s.inputs != 0x34) {
            cout << "get-status: incorrect inputs state - expected:" << 0x34 << ", got:" << s.inputs << endl;
            ok = false;
        }

        if (s.syserror != 0x56) {
            cout << "get-status: incorrect system error - expected:" << 0x56 << ", got:" << s.syserror << endl;
            ok = false;
        }

        if (s.info != 253) {
            cout << "get-status: incorrect special info - expected:" << 253 << ", got:" << s.info << endl;
            ok = false;
        }

        if (s.seqno != 9876) {
            cout << "get-status: incorrect sequence number - expected:" << 9876 << ", got:" << s.seqno << endl;
            ok = false;
        }

        if (string(s.event.timestamp) != "2022-01-02 12:34:56") {
            cout << "get-status: incorrect event timestamp - expected:"
                 << "2022-01-02 12:34:56"
                 << ", got:" << s.event.timestamp << endl;
            ok = false;
        }

        if (s.event.index != 135) {
            cout << "get-status: incorrect event index - expected:" << 135 << ", got:" << s.event.index << endl;
            ok = false;
        }

        if (s.event.eventType != 6) {
            cout << "get-status: incorrect event type - expected:" << 6 << ", got:" << s.event.eventType << endl;
            ok = false;
        }

        if (s.event.granted != 1) {
            cout << "get-status: incorrect event granted - expected:" << 1 << ", got:" << s.event.granted << endl;
            ok = false;
        }

        if (s.event.door != 3) {
            cout << "get-status: incorrect event door - expected:" << 3 << ", got:" << s.event.door << endl;
            ok = false;
        }

        if (s.event.direction != 1) {
            cout << "get-status: incorrect event direction - expected:" << 1 << ", got:" << s.event.direction << endl;
            ok = false;
        }

        if (s.event.card != 8100023) {
            cout << "get-status: incorrect event card - expected:" << 8100023 << ", got:" << s.event.card << endl;
            ok = false;
        }

        if (s.event.reason != 21) {
            cout << "get-status: incorrect event reason - expected:" << 21 << ", got:" << s.event.reason << endl;
            ok = false;
        }

        if (ok) {
            cout << "get-status:  ok" << endl;
        }

        return ok;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}
