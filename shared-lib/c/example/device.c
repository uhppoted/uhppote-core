#include <stdio.h>
#include <stdlib.h>

#include "../include/uhppote.h"

int getDevices() {
    unsigned long *devices = NULL;
    int N;

    if (get_devices(&devices, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    } 

    printf("\nget-devices (%d)\n", N);
    if (N > 0 && devices != NULL) {
        for (int i=0; i<N; i++) {
            printf("   %lu\n", devices[i]);        
        }
    }

    free(devices);

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
