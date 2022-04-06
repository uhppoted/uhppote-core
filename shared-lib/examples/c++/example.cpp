#include <ctime>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "cards.hpp"
#include "device.hpp"

using namespace std;

void usage();

extern const uint32_t DEVICEID = 405419896;
extern const uint32_t CARDID = 8000001;
extern const uint8_t DOOR = 4;

const controller ALPHA = {.id = 405419896, .address = "192.168.1.100"};
const controller BETA = {.id = 303986753, .address = "192.168.1.100"};

typedef int(f)(int, char **a);

typedef struct command {
    string cmd;
    f fn;
} command;

const vector<command> commands = {
    {.cmd = "get-devices"},
    {.cmd = "get-device"},
    {.cmd = "set-address"},
    {.cmd = "get-status"},
    {.cmd = "get-time"},
    {.cmd = "set-time"},
    {.cmd = "get-listener"},
    {.cmd = "get-door-control"},
    {.cmd = "set-door-control"},
    {.cmd = "get-cards"},
    {.cmd = "get-card"},
};

int main(int argc, char **argv) {
    if (argc < 2) {
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
    } else if (cmd == "get-door-control") {
        return getDoorControl(u, DEVICEID, DOOR);
    } else if (cmd == "set-door-control") {
        return setDoorControl(u, DEVICEID, DOOR, NORMALLY_OPEN, 9);
    } else if (cmd == "get-cards") {
        return getCards(u, argc, argv);
    } else if (cmd == "get-card") {
        return getCard(u, argc, argv);
    }

    cerr << endl
         << "   *** ERROR invalid command '" << cmd << "'" << endl;

    usage();

    return -1;
}

void usage() {
    cout << endl;
    cout << "   Usage: example <command>" << endl;
    cout << endl;
    cout << "   Supported commands:" << endl;

    for (auto it = commands.begin(); it != commands.end(); it++) {
        cout << "      " << it->cmd << endl;
    }

    cout << endl;
}
