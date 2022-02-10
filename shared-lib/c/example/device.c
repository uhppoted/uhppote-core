#include <stdio.h>
#include <stdlib.h>

#include "../include/uhppote.h"

int getDevices() {
    uint32_t *devices = NULL;
    int N;

    if (get_devices(&devices, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    } 

    printf("\nget-devices (%d)\n", N);
    if (N > 0 && devices != NULL) {
        for (int i=0; i<N; i++) {
            printf("   %u\n", devices[i]);        
        }
    }
    printf("\n");

    free(devices);

    return 0;
}

int getDevice(uint32_t deviceID) {
    struct device d;

    if (get_device(deviceID, &d) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    } 
      
    printf("\nget-device\n");
    printf("  ID:      %u\n", d.ID);
    printf("  IP:      %s  %s  %s\n",  d.address,d.subnet,d.gateway);
    printf("  MAC:     %s\n",  d.MAC);
    printf("  version: %s\n",  d.version);
    printf("  date:    %s\n",  d.date);
    printf("\n");

    return 0;
}
