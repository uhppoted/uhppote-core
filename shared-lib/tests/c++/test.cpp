#include <iostream>

#include "../include/uhppoted.hpp"
#include "device.hpp"

using namespace std;

void usage();

const controller ALPHA = {.id = 405419896, .address = "192.168.1.100"};
const controller BETA = {.id = 303986753, .address = "192.168.1.100"};

int main(int argc, char **argv) {
    if (argc < 2) {
        cerr << endl
             << "*** ERROR missing command" << endl
             << endl;
        usage();
        return -1;
    }

    string cmd(argv[1]);
    vector<controller> controllers = {ALPHA, BETA};

    uhppoted u("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2, controllers, true);

    bool ok = true;

    if (cmd == "get-devices") {
        ok = getDevices(u);
    } else if (cmd == "get-device") {
        ok = getDevice(u);
    } else if (cmd == "set-address") {
        ok = setAddress(u);
    } else if (cmd == "get-status") {
        ok = getStatus(u);
    } else if (cmd == "get-time") {
        ok = getTime(u);
    } else if (cmd == "all") {
        ok = getDevices(u) ? ok : false;
        ok = getDevice(u) ? ok : false;
        ok = setAddress(u) ? ok : false;
        ok = getStatus(u) ? ok : false;
        ok = getTime(u) ? ok : false;
    }

    return ok ? 0 : -1;
}

void usage() {
    cout << "Usage: test <command>" << endl;
    cout << endl;
    cout << "   Supported commands:" << endl;
    cout << "      get-devices" << endl;
    cout << "      get-device" << endl;
    cout << "      set-address" << endl;
    cout << "      get-status" << endl;
    cout << endl;
}
