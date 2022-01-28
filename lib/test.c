#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "libuhppote.h"

int main(int argc, char **argv) {
    char *path = "qwerty";
    
    struct InterOp_return r = InterOp(path);
    printf("value: %lld\n", r.r0);
    printf("err:   %s\n", r.r1);
    
    struct Devices devices = GetDevices();
    printf("value: %d\n", devices.X);
    printf("value: %p\n", devices.Z);
    printf("value: %d\n", devices.Z[0]);

    struct Device device = GetDevice(405419896);
    printf("value: %u\n", device.ID);

    return 0;
}