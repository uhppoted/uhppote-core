#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "libuhppote.h"

int main(int argc, char **argv) {
    // char *path = "qwerty";
    
    // struct InterOp_return r = InterOp(path);
    // printf("value: %lld\n", r.r0);
    // printf("err:   %s\n", r.r1);
    
    unsigned long list[5];
    int l = sizeof(list)/sizeof(unsigned long);
    GoSlice slice = { &list,l,l} ;

    struct GetDevices_return rc = GetDevices(slice);
    printf("N:   %d\n", rc.r0);
    printf("err: %s\n", rc.r1);

    int N = rc.r0 < 5 ? rc.r0 : l;
    for (int i=0; i<N; i++) {
        printf("  device[%d]: %lu\n", i,list[i]);        
    }

    struct Device device = GetDevice(405419896);
    printf("value: %lu\n", device.ID);

    return 0;
}