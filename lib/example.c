#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "uhppote.h"

int getDevices();
int getDevice();

int main(int argc, char **argv) {
    if (argc > 1) {
        char *cmd = argv[1];

        if (strncmp(cmd,"get-devices",11) == 0) {
            return getDevices();
        }

        if (strncmp(cmd,"get-device",10) == 0) {
            return getDevice();
        }
    }

    return -1;
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
    printf("  IP:      %s,%s,%s\n",  d.address,d.subnet,d.gateway);
    printf("  MAC:     %s\n",  d.MAC);
    printf("  version: %s\n",  d.version);
    printf("  date:    %s\n",  d.date);
    printf("\n");

    return 0;
}
