#include <ctime>
#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "cards.hpp"
#include "device.hpp"

using namespace std;

void usage();
void help();

extern const uint32_t DEVICE_ID = 405419896;
extern const uint32_t CARD_NUMBER = 8000001;
extern const uint32_t CARD_INDEX = 7;
extern const uint8_t DOOR = 4;

const controller ALPHA = {.id = 405419896, .address = "192.168.1.100"};
const controller BETA = {.id = 303986753, .address = "192.168.1.100"};

typedef int(f)(uhppoted &, int, char **);

typedef struct command {
    string cmd;
    string help;
    f *fn;
} command;

const vector<command> commands = {
    {
        .cmd = "get-devices",
        .help = "Retrieves a list of UHPPOTE controller IDs findable on the local LAN.",
        .fn = getDevices,
    },
    {
        .cmd = "get-device",
        .help = "Retrieves the basic device information for a single UHPPOTE controller.",
        .fn = getDevice,
    },
    {.cmd = "set-address",
     .help = "Sets the controller IPv4 address, subnet mask and gateway address."},
    {.cmd = "get-status",
     .help = "Retrieves a controller status."},
    {.cmd = "get-time",
     .help = "Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss)."},
    {.cmd = "set-time",
     .help = "Sets a controller current date/time (YYYY-MM-DD HH:mm:ss)."},
    {.cmd = "get-listener",
     .help = "Retrieves a controller's configured event listener address."},
    {.cmd = "set-listener",
     .help = "Configures a controller's event listener address and port."},
    {.cmd = "get-door-control",
     .help = "Retrieves the control state and open delay for a controller door."},
    {.cmd = "set-door-control",
     .help = "Sets the control mode and delay for a controller door."},
    {
        .cmd = "get-cards",
        .help = "Retrieves the number of cards stored on a controller.",
        .fn = getCards,
    },
    {
        .cmd = "get-card",
        .help = "Retrieves the card detail for card number from a controller.",
        .fn = getCard,
    },
    {
        .cmd = "get-card-by-index",
        .help = "Retrieves the card detail for the card stored at an index on a controller.",
        .fn = getCardByIndex,
    },
    {
        .cmd = "put-card",
        .help = "Adds or updates the card detail for card number stored on a controller.",
        .fn = putCard,
    },
    {
        .cmd = "delete-card",
        .help = "Deletes a card from a controller.",
        .fn = deleteCard,
    },
};

int main(int argc, char **argv) {
    if (argc < 2) {
        usage();
        return -1;
    }

    string cmd(argv[1]);
    vector<controller> controllers = {ALPHA, BETA};

    uhppoted u("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2500, controllers, true);

    if (cmd == "help") {
        help();
        return 0;
    }

    if (cmd == "set-address") {
        return setAddress(u, DEVICE_ID, "192.168.1.125", "255.255.254.0", "192.168.1.10");
    } else if (cmd == "get-status") {
        return getStatus(u, DEVICE_ID);
    } else if (cmd == "get-time") {
        return getTime(u, DEVICE_ID);
    } else if (cmd == "set-time") {
        time_t now = time(nullptr);
        char datetime[100];

        strftime(datetime, sizeof(datetime), "%Y-%m-%d %H:%M:%S", localtime(&now));

        return setTime(u, DEVICE_ID, datetime);
    } else if (cmd == "get-listener") {
        return getListener(u, DEVICE_ID);
    } else if (cmd == "set-listener") {
        return setListener(u, DEVICE_ID, "192.168.1.100:60001");
    } else if (cmd == "get-door-control") {
        return getDoorControl(u, DEVICE_ID, DOOR);
    } else if (cmd == "set-door-control") {
        return setDoorControl(u, DEVICE_ID, DOOR, NORMALLY_OPEN, 9);
    } else {
        for (auto it = commands.begin(); it != commands.end(); it++) {
            if (it->cmd == cmd) {
                return it->fn(u, argc, argv);
            }
        }
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

void help() {
    cout << endl;
    cout << "   Usage: example <command>" << endl;
    cout << endl;
    cout << "   Commands:" << endl;

    for (auto it = commands.begin(); it != commands.end(); it++) {
        cout << "      " << setw(18) << left << it->cmd << "  " << it->help << endl;
    }

    cout << endl;
}
