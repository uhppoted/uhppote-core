#include <ctime>
#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "examples.hpp"

using namespace std;

void usage();
void help();

extern const uint32_t DEVICE_ID = 405419896;
extern const uint32_t CARD_NUMBER = 8000001;
extern const uint32_t CARD_INDEX = 7;
extern const uint32_t EVENT_INDEX = 7;
extern const uint8_t DOOR = 4;
extern const uint8_t PROFILE_ID = 29;

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
    {
        .cmd = "set-address",
        .help = "Sets the controller IPv4 address, subnet mask and gateway address.",
        .fn = setAddress,
    },
    {
        .cmd = "get-status",
        .help = "Retrieves a controller status.",
        .fn = getStatus,
    },
    {
        .cmd = "get-time",
        .help = "Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss).",
        .fn = getTime,
    },
    {
        .cmd = "set-time",
        .help = "Sets a controller current date/time (YYYY-MM-DD HH:mm:ss).",
        .fn = setTime,
    },
    {
        .cmd = "get-listener",
        .help = "Retrieves a controller's configured event listener address.",
        .fn = getListener,
    },
    {
        .cmd = "set-listener",
        .help = "Configures a controller's event listener address and port.",
        .fn = setListener,
    },
    {
        .cmd = "get-door-control",
        .help = "Retrieves the control state and open delay for a controller door.",
        .fn = getDoorControl,
    },
    {
        .cmd = "set-door-control",
        .help = "Sets the control mode and delay for a controller door.",
        .fn = setDoorControl,
    },
    {
        .cmd = "open-door",
        .help = "Remotely opens a controller door.",
        .fn = openDoor,
    },
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
    {
        .cmd = "delete-cards",
        .help = "Deletes all cards from a controller.",
        .fn = deleteCards,
    },
    {
        .cmd = "get-event-index",
        .help = "Retrieves the current event index from a controller.",
        .fn = getEventIndex,
    },
    {
        .cmd = "set-event-index",
        .help = "Sets the current event index on a controller.",
        .fn = setEventIndex,
    },
    {
        .cmd = "get-event",
        .help = "Retrieves the event at the index from a controller.",
        .fn = getEvent,
    },
    {
        .cmd = "record-special-events",
        .help = "Enables/disables recording additional events for a controller.",
        .fn = recordSpecialEvents,
    },
    {
        .cmd = "get-time-profile",
        .help = "Retrieves a time profile from a controller.",
        .fn = getTimeProfile,
    },
    {
        .cmd = "set-time-profile",
        .help = "Adds or update a time profile on a controller.",
        .fn = setTimeProfile,
    },
};

int main(int argc, char **argv) {
    if (argc < 2) {
        usage();
        return -1;
    }

    string cmd(argv[1]);
    if (cmd == "help") {
        help();
        return 0;
    }

    for (auto it = commands.begin(); it != commands.end(); it++) {
        if (it->cmd == cmd) {
            uhppoted u("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2500, {ALPHA, BETA}, true);

            return it->fn(u, argc, argv);
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
