#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "../include/uhppote.hpp"

void usage();
extern int getDevices(uhppote&);
extern int getDevice(uhppote&);

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("\n*** ERROR missing command\n\n");
        usage();
        return -1;
    }

    char *cmd = argv[1];

    controller alpha = { .id=405419896, .address="192.168.1.100" };
    controller beta  = { .id=303986753, .address="192.168.1.100" };

    uhppote u("192.168.1.100:0","192.168.1.255:60000","192.168.1.100:60001", 2, true, &alpha, &beta, NULL);

    if (strncmp(cmd,"get-devices",11) == 0) {
        return getDevices(u);
    } 

    if (strncmp(cmd,"get-device",10) == 0) {
        return getDevice(u);
    }
}

void usage() {
    printf("Usage: example <command>\n");
    printf("\n");
    printf("   Supported commands:\n");
    printf("      get-devices\n");
    printf("      get-device\n");
    printf("\n");
}
