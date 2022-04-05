#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "cards.h"
#include "device.h"
#include "uhppoted.h"

void usage();

const uint32_t DEVICEID = 405419896;
const uint8_t DOOR = 4;

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

    if (strncmp(cmd, "get-devices", 11) == 0) {
        rc = getDevices();
    } else if (strncmp(cmd, "get-device", 10) == 0) {
        rc = getDevice(DEVICEID);
    } else if (strncmp(cmd, "set-address", 11) == 0) {
        rc = setAddress(DEVICEID, "192.168.1.125", "255.255.254.0", "192.168.1.0");
    } else if (strncmp(cmd, "get-status", 10) == 0) {
        rc = getStatus(DEVICEID);
    } else if (strncmp(cmd, "get-time", 8) == 0) {
        rc = getTime(DEVICEID);
    } else if (strncmp(cmd, "set-time", 8) == 0) {
        time_t utc;
        struct tm *local;
        char datetime[20];

        time(&utc);
        local = localtime(&utc);

        strftime(datetime, 20, "%Y-%m-%d %H:%M:%S", local);

        rc = setTime(DEVICEID, datetime);
    } else if (strncmp(cmd, "get-listener", 12) == 0) {
        rc = getListener(DEVICEID);
    } else if (strncmp(cmd, "set-listener", 12) == 0) {
        rc = setListener(DEVICEID, "192.168.1.100:60001");
    } else if (strncmp(cmd, "get-door-control", 16) == 0) {
        rc = getDoorControl(DEVICEID, DOOR);
    } else if (strncmp(cmd, "set-door-control", 16) == 0) {
        rc = setDoorControl(DEVICEID, DOOR, NORMALLY_OPEN, 9);
    } else if (strncmp(cmd, "get-cards", 16) == 0) {
        rc = getCards(argc, argv);
    } else {
        printf("\n   *** ERROR invalid command (%s)\n\n", cmd);
        usage();
        rc = -1;
    }

    teardown();

    return rc;
}

void usage() {
    printf("   Usage: example <command>\n");
    printf("\n");
    printf("   Supported commands:\n");
    printf("      get-devices\n");
    printf("      get-device\n");
    printf("      set-address\n");
    printf("      get-status\n");
    printf("      get-time\n");
    printf("      set-time\n");
    printf("      get-listener\n");
    printf("      set-listener\n");
    printf("      get-door-control\n");
    printf("      set-door-control\n");
    printf("      get-cards\n");
    printf("\n");
}
