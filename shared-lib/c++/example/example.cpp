#include <iostream>

#include "../include/uhppoted.hpp"
#include "device.hpp"

using namespace std;

void usage();

int main(int argc, char **argv) {
    if (argc < 2) {
        cerr << endl
             << "*** ERROR missing command" << endl
             << endl;
        usage();
        return -1;
    }

    string cmd(argv[1]);

    controller alpha = {.id = 405419896, .address = "192.168.1.100"};
    controller beta = {.id = 303986753, .address = "192.168.1.100"};
    vector<controller> controllers = {alpha, beta};

    uhppoted u("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2, controllers, true);

    if (cmd == "get-devices") {
        return getDevices(u);
    } else if (cmd == "get-device") {
        return getDevice(u, 405419896);
    } else if (cmd == "set-address") {
        return setAddress(u, 405419896, "192.168.1.125", "255.255.254.0", "192.168.1.10");
    } else if (cmd == "all") {
        getDevices(u);
        getDevice(u, 405419896);
        setAddress(u, 405419896, "192.168.1.125", "255.255.254.0", "192.168.1.10");
        return 0;
    }

    cerr << endl
         << "*** ERROR invalid command '" << cmd << "'" << endl
         << endl;

    return -1;
}

void usage() {
    cout << "Usage: example <command>" << endl;
    cout << endl;
    cout << "   Supported commands:" << endl;
    cout << "      get-devices" << endl;
    cout << "      get-device" << endl;
    cout << "      set-address" << endl;
    cout << endl;
}
