#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "device.h"
#include "uhppoted.h"

void usage();

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("\n*** ERROR missing command\n\n");
        usage();
        return -1;
    }

    bool ok = true;
    char *cmd = argv[1];

    controller alpha = {.id = 405419896, .address = "192.168.1.100"};
    controller beta = {.id = 303986753, .address = "192.168.1.100"};

    setup("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2, true, &alpha, &beta, NULL);

    if (strncmp(cmd, "get-devices", 11) == 0) {
        ok = getDevices();
    } else if (strncmp(cmd, "get-device", 10) == 0) {
        ok = getDevice();
    } else if (strncmp(cmd, "set-address", 11) == 0) {
        ok = setAddress();
    } else if (strncmp(cmd, "get-status", 10) == 0) {
        ok = getStatus();
    } else if (strncmp(cmd, "get-time", 8) == 0) {
        ok = getTime();
    } else if (strncmp(cmd, "all", 3) == 0) {
        ok = getDevices() ? ok : false;
        ok = getDevice() ? ok : false;
        ok = setAddress() ? ok : false;
        ok = getStatus() ? ok : false;
        ok = getTime() ? ok : false;
    }

    teardown();

    return ok ? 0 : -1;
}

void usage() {
    printf("Usage: test <command>\n");
    printf("\n");
    printf("   Supported commands:\n");
    printf("      get-devices\n");
    printf("      get-device\n");
    printf("      set-address\n");
    printf("      get-status\n");
    printf("\n");
}
