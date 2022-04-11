#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "cards.h"
#include "device.h"
#include "uhppoted.h"

void usage();
void help();

const uint32_t DEVICE_ID = 405419896;
const uint8_t DOOR = 4;
const uint32_t CARD_NUMBER = 8000001;
const uint32_t CARD_INDEX = 7;

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
        .fn = getCard,
    },
};

int main(int argc, char **argv) {
    if (argc < 2) {
        usage();
        return -1;
    }

    char *cmd = argv[1];
    int rc = -1;

    controller alpha = {.id = 405419896, .address = "192.168.1.100"};
    controller beta = {.id = 303986753, .address = "192.168.1.100"};

    setup("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2500, true, &alpha, &beta, NULL);

    if (strcmp(cmd, "help") == 0) {
        help();
        return 0;
    }

    if (strncmp(cmd, "get-status", 10) == 0) {
        rc = getStatus(DEVICE_ID);
    } else if (strncmp(cmd, "get-time", 8) == 0) {
        rc = getTime(DEVICE_ID);
    } else if (strncmp(cmd, "set-time", 8) == 0) {
        time_t utc;
        struct tm *local;
        char datetime[20];

        time(&utc);
        local = localtime(&utc);

        strftime(datetime, 20, "%Y-%m-%d %H:%M:%S", local);

        rc = setTime(DEVICE_ID, datetime);
    } else if (strncmp(cmd, "get-listener", 12) == 0) {
        rc = getListener(DEVICE_ID);
    } else if (strncmp(cmd, "set-listener", 12) == 0) {
        rc = setListener(DEVICE_ID, "192.168.1.100:60001");
    } else if (strncmp(cmd, "get-door-control", 16) == 0) {
        rc = getDoorControl(DEVICE_ID, DOOR);
    } else if (strncmp(cmd, "set-door-control", 16) == 0) {
        rc = setDoorControl(DEVICE_ID, DOOR, NORMALLY_OPEN, 9);
    } else {
        int N = sizeof(commands) / sizeof(command);
        for (int i = 0; i < N; i++) {
            command c = commands[i];

            if (strcmp(c.cmd, cmd) == 0) {
                rc = c.fn(argc, argv);
                goto done;
            }
        }

        printf("\n   *** ERROR invalid command (%s)\n", cmd);
        usage();
        rc = -1;
    }

done:
    teardown();

    return rc;
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
