#include <ctime>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "device.hpp"

using namespace std;

void usage();

const uint32_t DEVICEID = 405419896;
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

    uhppoted u("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2500, controllers, true);

    if (cmd == "get-devices") {
        return getDevices(u);
    } else if (cmd == "get-device") {
        return getDevice(u, DEVICEID);
    } else if (cmd == "set-address") {
        return setAddress(u, DEVICEID, "192.168.1.125", "255.255.254.0", "192.168.1.10");
    } else if (cmd == "get-status") {
        return getStatus(u, DEVICEID);
    } else if (cmd == "get-time") {
        return getTime(u, DEVICEID);
    } else if (cmd == "set-time") {
        time_t now = time(nullptr);
        char datetime[100];

        strftime(datetime, sizeof(datetime), "%Y-%m-%d %H:%M:%S", localtime(&now));

        return setTime(u, DEVICEID, datetime);
    } else if (cmd == "get-listener") {
        return getListener(u, DEVICEID);
    } else if (cmd == "set-listener") {
        return setListener(u, DEVICEID, "192.168.1.100:60001");
    } else if (cmd == "all") {
        time_t now = time(nullptr);
        char datetime[100];

        strftime(datetime, sizeof(datetime), "%Y-%m-%d %H:%M:%S", localtime(&now));

        int rc = 0;

        rc = getDevices(u) == 0 ? rc : -1;
        rc = getDevice(u, DEVICEID) == 0 ? rc : -1;
        rc = setAddress(u, DEVICEID, "192.168.1.125", "255.255.254.0", "192.168.1.10") == 0 ? rc : -1;
        rc = getStatus(u, DEVICEID) == 0 ? rc : -1;
        rc = getTime(u, DEVICEID) == 0 ? rc : -1;
        rc = setTime(u, DEVICEID, datetime) == 0 ? rc : -1;
        rc = getListener(u, DEVICEID) == 0 ? rc : -1;
        rc = setListener(u, DEVICEID, "192.168.1.100:60001") == 0 ? rc : -1;

        return rc;
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
    cout << "      get-status" << endl;
    cout << "      get-time" << endl;
    cout << "      set-time" << endl;
    cout << "      get-listener" << endl;
    cout << endl;
}
