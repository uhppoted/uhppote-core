#include <iostream>
#include <map>

#include "../include/uhppoted.hpp"
#include "device.hpp"

using namespace std;

typedef bool (*f)(uhppoted &);

map<string, f> tests = {
    {"get-devices", getDevices},
    {"get-device", getDevice},
    {"set-address", setAddress},
    {"get-status", getStatus},
    {"get-time", getTime},
    {"set-time", setTime},
};

void usage();

const controller ALPHA = {.id = 405419896, .address = "192.168.1.100"};
const controller BETA = {.id = 303986753, .address = "192.168.1.100"};

int main(int argc, char **argv) {
    string cmd;

    if (argc > 1) {
        cmd = argv[1];
    }

    vector<controller> controllers = {ALPHA, BETA};

    uhppoted u("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2, controllers, true);

    if (cmd == "" || cmd == "all") {
        bool ok = true;
        for (auto it = tests.begin(); it != tests.end(); it++) {
            ok = it->second(u) ? ok : false;
        }

        return ok ? 0 : -1;
    }

    auto it = tests.find(cmd);
    if (it == tests.end()) {
        cerr << endl
             << "   *** ERROR invalid command (" << cmd << ")" << endl
             << endl;
        usage();
        return -1;
    }

    return it->second(u) ? 0 : -1;
}

void usage() {
    cout << "   Usage: test <command>" << endl;
    cout << endl;
    cout << "   Supported commands:" << endl;
    cout << "      get-devices" << endl;
    cout << "      get-device" << endl;
    cout << "      set-address" << endl;
    cout << "      get-status" << endl;
    cout << "      get-time" << endl;
    cout << "      set-time" << endl;
    cout << endl;
}
