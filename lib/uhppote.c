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

UHPPOTE *setup() {
    UHPPOTE *u;

    if ((u = (UHPPOTE *) malloc(sizeof(UHPPOTE))) != NULL) {
        u->bind = "192.168.1.100";
        u->broadcast = "192.168.1.255";
        u->listen = "192.168.1.100:60001";
        u->timeout = 5;
        u->devices = NULL;
        u->debug = true;

        udevice *alpha;
        udevice *beta;

        if ((alpha = (udevice *) malloc(sizeof(udevice))) != NULL) {
            alpha->id=405419896;
            alpha->address="192.168.1.100";
            alpha->next=NULL;

            u->devices = alpha;

            if ((beta = (udevice *) malloc(sizeof(udevice))) != NULL) {
                beta->id=303986753;
                beta->address="192.168.1.100";
                beta->next=NULL;

                alpha->next = beta;
            }
        }
    };

    return u;
}

int teardown(UHPPOTE *u) {
    udevice *d = u->devices;

    while (d != NULL) {
        udevice *next = d->next;
        free(d);
        d = next;
    }

    free(u);

    return 0;
}

void set_error(const char *errmsg) {
    unsigned l = strlen(errmsg) + 1;

    if (err != NULL) {
        free(err);
    }

    if ((err = malloc(l)) != NULL) {
        snprintf(err, l, "%s", errmsg);            
    }
}

int get_devices(int N, unsigned long list[]) {
    UHPPOTE *u = setup();

    GoSlice slice = { list,N,N} ;

    struct GetDevices_return rc = GetDevices(u, slice);
    if (rc.r1 != NULL) {
        set_error(rc.r1);
        teardown(u);
        return -1;
    }

    teardown(u);

    return rc.r0;
}

int get_device(unsigned id, struct device *d) {
    UHPPOTE *u = setup();

    struct GetDevice_return rc = GetDevice(u,id);

    if (rc.r1 != NULL) {        
        set_error(rc.r1);
        teardown(u);
        return -1;
    }

    d->ID = rc.r0.ID;
    snprintf(d->address, sizeof(d->address), "%s", rc.r0.address);
    snprintf(d->subnet,  sizeof(d->subnet),  "%s", rc.r0.subnet);
    snprintf(d->gateway, sizeof(d->gateway), "%s", rc.r0.gateway);
    snprintf(d->MAC,     sizeof(d->MAC),     "%s", rc.r0.MAC);
    snprintf(d->version, sizeof(d->version), "%s", rc.r0.version);
    snprintf(d->date,    sizeof(d->date),    "%s", rc.r0.date);

    teardown(u);
    return 0;
}