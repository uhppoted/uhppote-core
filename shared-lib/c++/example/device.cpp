#include "../include/uhppoted.hpp"
#include <iostream>

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
        cout << "  IP:       " << d.address << "  " << d.subnet << "  " << d.gateway << endl;
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
