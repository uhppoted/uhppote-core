#include <stdio.h>

#include "../include/uhppote.h"

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
