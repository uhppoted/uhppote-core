#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "device.h"
#include "uhppoted.h"

void usage();

const uint32_t DEVICEID = 405419896;

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("\n*** ERROR missing command\n\n");
        usage();
        return -1;
    }

    int rc = -1;
    char *cmd = argv[1];

    controller alpha = {.id = 405419896, .address = "192.168.1.100"};
    controller beta = {.id = 303986753, .address = "192.168.1.100"};

    setup("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2,
          true, &alpha, &beta, NULL);

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
    } else if (strncmp(cmd, "all", 3) == 0) {
        rc = 0;
        rc = getDevices() == 0 ? rc : -1;
        rc = getDevice(DEVICEID) == 0 ? rc : -1;
        rc = setAddress(DEVICEID, "192.168.1.125", "255.255.254.0", "192.168.1.0") == 0 ? rc : -1;
        rc = getStatus(DEVICEID) == 0 ? rc : -1;
        rc = getTime(DEVICEID) == 0 ? rc : -1;
    }

    teardown();

    return rc;
}

void usage() {
    printf("Usage: example <command>\n");
    printf("\n");
    printf("   Supported commands:\n");
    printf("      get-devices\n");
    printf("      get-device\n");
    printf("      set-address\n");
    printf("      get-status\n");
    printf("      get-time\n");
    printf("\n");
}
