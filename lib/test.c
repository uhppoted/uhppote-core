#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "uhppote.h"

int main(int argc, char **argv) {
    if (argc > 1) {
        char *cmd = argv[1];

        if (strncmp(cmd,"get-device",10) == 0) {
            struct device d;

            if (get_device(405419896, &d) != 0) {
                printf("ERROR %s\n", errmsg());
            } else{
                printf("ID:         %lu\n", d.ID);
                printf("IP address: %s\n",  d.address);
                printf("   subnet:  %s\n",  d.subnet);
                printf("   gateway: %s\n",  d.gateway);
                printf("MAC:        %s\n",  d.MAC);
                printf("version:    %s\n",  d.version);
                printf("date:       %s\n",  d.date);
            }
        }
    }

    // unsigned long list[5];
    // int l = sizeof(list)/sizeof(unsigned long);
    // GoSlice slice = { &list,l,l} ;

    // struct GetDevices_return rc = GetDevices(slice);
    // printf("N:   %d\n", rc.r0);
    // printf("err: %s\n", rc.r1);

    // int N = rc.r0 < 5 ? rc.r0 : l;
    // for (int i=0; i<N; i++) {
    //     printf("  device[%d]: %lu\n", i,list[i]);        
    // }

    // struct Device device = GetDevice(405419896);
    // printf("value: %lu\n", device.ID);

    // return 0;
}