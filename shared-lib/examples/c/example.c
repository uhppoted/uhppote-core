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
const uint32_t CARD_ID = 8000001;
const uint32_t CARD_INDEX = 7;

typedef int(f)(int, char **a);

typedef struct command {
    char *cmd;
    char *help;
    f *fn;
} command;

const command commands[] = {
    {.cmd = "get-devices",
     .help = "Retrieves a list of UHPPOTE controller IDs findable on the local LAN."},
    {.cmd = "get-device",
     .help = "Retrieves the basic device information for a single UHPPOTE controller."},
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
    {.cmd = "get-cards",
     .help = "Retrieves the number of cards stored on a controller."},
    {.cmd = "get-card",
     .help = "Retrieves the card detail for card number from a controller."},
    {.cmd = "get-card-by-index",
     .help = "Retrieves the card detail for the card stored at an index on a controller.",
     .fn = getCardByIndex},
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

    if (strncmp(cmd, "get-devices", 11) == 0) {
        rc = getDevices();
    } else if (strncmp(cmd, "get-device", 10) == 0) {
        rc = getDevice(argc, argv);
    } else if (strncmp(cmd, "set-address", 11) == 0) {
        rc = setAddress(DEVICE_ID, "192.168.1.125", "255.255.254.0", "192.168.1.0");
    } else if (strncmp(cmd, "get-status", 10) == 0) {
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
    } else if (strncmp(cmd, "get-cards", 16) == 0) {
        rc = getCards(argc, argv);
    } else if (strcmp(cmd, "get-card") == 0) {
        rc = getCard(argc, argv);
    } else if (strcmp(cmd, "get-card-by-index") == 0) {
        rc = getCardByIndex(argc, argv);
    } else {
        printf("\n   *** ERROR invalid command (%s)\n\n", cmd);
        usage();
        rc = -1;
    }

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
