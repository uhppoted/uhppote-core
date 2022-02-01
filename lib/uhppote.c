#include "uhppote.h"
#include "libuhppote.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

__thread char *err = NULL;

char *errmsg() {
    return err;
}

int get_device(unsigned id, struct device *d) {
    udevice alpha = { .id=405419896, .address="192.168.1.100", .next=NULL };
    udevice beta  = { .id=303986753, .address="192.168.1.100", .next=&alpha };

    UHPPOTE u = {
        .bind = "192.168.1.100",
        .broadcast = "192.168.1.255",
        .listen = "192.168.1.100:60001",
        .timeout = 5,
        .devices = &alpha,
        .debug = true,
    };

    struct GetDevice_return rc = GetDevice(u,id);

    if (rc.r1 != NULL) {        
        unsigned N = strlen(rc.r1) + 1;

        if (err != NULL) {
            free(err);
        }

        if ((err = malloc(N)) != NULL) {
            snprintf(err, N, "%s", rc.r1);            
        }

        return -1;
    }

    d->ID = rc.r0.ID;
    snprintf(d->address, sizeof(d->address), "%s", rc.r0.address);
    snprintf(d->subnet,  sizeof(d->subnet),  "%s", rc.r0.subnet);
    snprintf(d->gateway, sizeof(d->gateway), "%s", rc.r0.gateway);
    snprintf(d->MAC,     sizeof(d->MAC),     "%s", rc.r0.MAC);
    snprintf(d->version, sizeof(d->version), "%s", rc.r0.version);
    snprintf(d->date,    sizeof(d->date),    "%s", rc.r0.date);

    return 0;
}