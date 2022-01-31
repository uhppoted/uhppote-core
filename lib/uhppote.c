#include "uhppote.h"
#include "libuhppote.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

__thread char *err = NULL;

char *errmsg() {
    return err;
}

int get_device(unsigned id, struct device *d) {
    struct GetDevice_return rc = GetDevice(id);

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