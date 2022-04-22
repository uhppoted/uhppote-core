#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "examples.h"
#include "uhppoted.h"

void usage();
void help();

const uint32_t DEVICE_ID = 405419896;
const uint8_t DOOR = 4;
const uint32_t CARD_NUMBER = 8000001;
const uint32_t CARD_INDEX = 7;
const uint32_t EVENT_INDEX = 91;
const uint8_t PROFILE_ID = 29;

typedef int(f)(int, char **a);

typedef struct command {
    char *cmd;
    char *help;
    f *fn;
} command;

const command commands[] = {
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
        .fn = getCard,
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
        .help = "Adds or updates a time profile on a controller.",
        .fn = setTimeProfile,
    },
};

controller alpha = {.id = 405419896, .address = "192.168.1.100"};
controller beta = {.id = 303986753, .address = "192.168.1.100"};

int main(int argc, char **argv) {
    if (argc < 2) {
        usage();
        return -1;
    }

    char *cmd = argv[1];

    if (strcmp(cmd, "help") == 0) {
        help();
        return 0;
    }

    int N = sizeof(commands) / sizeof(command);
    for (int i = 0; i < N; i++) {
        command c = commands[i];
        int rc;

        if (strcmp(c.cmd, cmd) == 0) {
            setup("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2500, true, &alpha, &beta, NULL);
            rc = c.fn(argc, argv);
            teardown();

            return rc;
        }
    }

    printf("\n   *** ERROR invalid command (%s)\n", cmd);
    usage();
    return -1;
}

void usage() {
    int N = sizeof(commands) / sizeof(command);

    printf("\n");
    printf("   Usage: example <command>\n");
    printf("\n");
    printf("   Supported commands:\n");

    for (int i = 0; i < N; i++) {
        printf("      %s\n", commands[i].cmd);
    }

    printf("\n");
}

void help() {
    int N = sizeof(commands) / sizeof(command);

    printf("\n");
    printf("   Usage: example <command>\n");
    printf("\n");
    printf("   Commands:\n");

    for (int i = 0; i < N; i++) {
        printf("      %-17s %s\n", commands[i].cmd, commands[i].help);
    }

    printf("\n");
}
