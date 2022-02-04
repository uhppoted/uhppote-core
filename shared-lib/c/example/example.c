#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "../include/uhppote.h"

void usage();
int getDevices();
int getDevice();

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("\n*** ERROR missing command\n\n");
        usage();
        return -1;
    }

    int rc = -1;
    char *cmd = argv[1];

    controller alpha = { .id=405419896, .address="192.168.1.100" };
    controller beta  = { .id=303986753, .address="192.168.1.100" };

    setup("192.168.1.100:0","192.168.1.255:60000","192.168.1.100:60001", 1, true, &alpha, &beta, NULL);

    if (strncmp(cmd,"get-devices",11) == 0) {
        rc = getDevices();
    }

    if (strncmp(cmd,"get-device",10) == 0) {
        rc = getDevice();
    }

    teardown();

    return rc;
}

int getDevices() {
    int N;
    int L = 10;
    unsigned long list[L];

    if ((N = get_devices(L, list)) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    } 

    printf("\nget-devices\n");
    printf("  N: %d\n", N);

    int l = N < L ? N : L;      
    for (int i=0; i<l; i++) {
        printf("     %lu\n", list[i]);        
    }
    
    printf("\n");

    return 0;
}

int getDevice() {
    struct device d;

    if (get_device(405419896, &d) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    } 
      
    printf("\nget-device\n");
    printf("  ID:      %lu\n", d.ID);
    printf("  IP:      %s  %s  %s\n",  d.address,d.subnet,d.gateway);
    printf("  MAC:     %s\n",  d.MAC);
    printf("  version: %s\n",  d.version);
    printf("  date:    %s\n",  d.date);
    printf("\n");

    return 0;
}

void usage() {
    printf("Usage: example <command>\n");
    printf("\n");
    printf("   Supported commands:\n");
    printf("      get-devices\n");
    printf("      get-device\n");
    printf("\n");
}
