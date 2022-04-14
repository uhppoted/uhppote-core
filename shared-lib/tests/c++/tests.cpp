#include <iomanip>
#include <iostream>
#include <map>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

typedef bool (*f)(uhppoted &);

typedef struct test {
    string command;
    f fn;
} test;

vector<test> tests = {
    {"get-devices", getDevices},
    {"get-device", getDevice},
    {"set-address", setAddress},
    {"get-status", getStatus},
    {"get-time", getTime},
    {"set-time", setTime},
    {"get-listener", getListener},
    {"set-listener", setListener},
    {"get-door-control", getDoorControl},
    {"set-door-control", setDoorControl},
    {"get-cards", getCards},
    {"get-card", getCard},
    {"get-card-by-index", getCardByIndex},
    {"put-card", putCard},
    {"delete-card", deleteCard},
    {"delete-cards", deleteCards},
    {"get-event-index", getEventIndex},
    {"set-event-index", setEventIndex},
};

extern const uint32_t DEVICE_ID = 405419896;
extern const uint32_t CARD_ID = 8165538;
extern const uint32_t CARD_INDEX = 19;
extern const uint32_t EVENT_INDEX = 51;
extern const uint8_t DOOR = 4;

extern bool result(string test, bool ok) {
    if (ok) {
        cout << setw(18) << left << test << "ok" << endl;
    }

    return ok;
}

const controller ALPHA = {.id = 405419896, .address = "192.168.1.100"};
const controller BETA = {.id = 303986753, .address = "192.168.1.100"};

void usage();

int main(int argc, char **argv) {
    string cmd;

    if (argc > 1) {
        cmd = argv[1];
    }

    vector<controller> controllers = {ALPHA, BETA};

    uhppoted u("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2500, controllers, true);

    if (cmd == "help") {
        cout << endl;
        usage();
        return 0;
    }

    if (cmd == "" || cmd == "all") {
        bool ok = true;
        for (auto it = tests.begin(); it != tests.end(); it++) {
            ok = it->fn(u) ? ok : false;
        }

        return ok ? 0 : -1;
    }

    for (auto it = tests.begin(); it != tests.end(); it++) {
        if (it->command == cmd) {
            return it->fn(u) ? 0 : -1;
        }
    }

    cerr << endl
         << "   *** ERROR invalid command (" << cmd << ")" << endl
         << endl;
    usage();
    return -1;
}

void usage() {
    cout << "   Usage: test <command>" << endl;
    cout << endl;
    cout << "   Supported commands:" << endl;

    for (auto it = tests.begin(); it != tests.end(); it++) {
        cout << "      " << it->command << endl;
    }
    cout << endl;
}