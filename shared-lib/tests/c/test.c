#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "device.h"
#include "uhppoted.h"

void usage();
bool all();

int main(int argc, char **argv) {
    bool ok = true;
    char *cmd;

    if (argc > 1) {
        cmd = argv[1];
    }

    controller alpha = {.id = 405419896, .address = "192.168.1.100"};
    controller beta = {.id = 303986753, .address = "192.168.1.100"};

    setup("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2, true, &alpha, &beta, NULL);

    if (cmd == NULL || strncmp(cmd, "all", 3) == 0) {
        ok = all();
    } else if (strncmp(cmd, "get-devices", 11) == 0) {
        ok = getDevices();
    } else if (strncmp(cmd, "get-device", 10) == 0) {
        ok = getDevice();
    } else if (strncmp(cmd, "set-address", 11) == 0) {
        ok = setAddress();
    } else if (strncmp(cmd, "get-status", 10) == 0) {
        ok = getStatus();
    } else if (strncmp(cmd, "get-time", 8) == 0) {
        ok = getTime();
    } else if (strncmp(cmd, "set-time", 8) == 0) {
        ok = setTime();
    } else {
        printf("\n*** ERROR invalid command (%s)\n\n", cmd);
        usage();
        ok = false;
    }

    teardown();

    return ok ? 0 : -1;
}

bool all() {
    bool ok = true;

    ok = getDevices() ? ok : false;
    ok = getDevice() ? ok : false;
    ok = setAddress() ? ok : false;
    ok = getStatus() ? ok : false;
    ok = getTime() ? ok : false;
    ok = setTime() ? ok : false;

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
    printf("      get-time\n");
    printf("      set-time\n");
    printf("\n");
}
