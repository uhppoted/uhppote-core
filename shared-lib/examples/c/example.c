#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "device.h"
#include "uhppoted.h"

int all();
void usage();

const uint32_t DEVICEID = 405419896;

int main(int argc, char **argv) {
    char *cmd = "";

    if (argc > 1) {
        cmd = argv[1];
    }

    int rc = -1;

    controller alpha = {.id = 405419896, .address = "192.168.1.100"};
    controller beta = {.id = 303986753, .address = "192.168.1.100"};

    setup("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2500, true, &alpha, &beta, NULL);

    if (strcmp(cmd, "") == 0 || strncmp(cmd, "all", 3) == 0) {
        rc = all();
    } else if (strncmp(cmd, "get-devices", 11) == 0) {
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
    } else {
        printf("\n   *** ERROR missing command\n\n");
        usage();
        rc = -1;
    }

    teardown();

    return rc;
}

int all() {
    int rc = -1;

    time_t utc;
    struct tm *local;
    char datetime[20];

    time(&utc);
    local = localtime(&utc);

    strftime(datetime, 20, "%Y-%m-%d %H:%M:%S", local);

    rc = 0;
    rc = getDevices() == 0 ? rc : -1;
    rc = getDevice(DEVICEID) == 0 ? rc : -1;
    rc = setAddress(DEVICEID, "192.168.1.125", "255.255.254.0", "192.168.1.0") == 0 ? rc : -1;
    rc = getStatus(DEVICEID) == 0 ? rc : -1;
    rc = getTime(DEVICEID) == 0 ? rc : -1;
    rc = setTime(DEVICEID, datetime) == 0 ? rc : -1;
    rc = getListener(DEVICEID) == 0 ? rc : -1;

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
    printf("\n");
}
