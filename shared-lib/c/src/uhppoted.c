#include "../include/uhppoted.h"
#include "libuhppoted.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

__thread char *err = NULL;
__thread UHPPOTE *u = NULL;

const char *errmsg() {
    return err;
}

/* (optional) setup for UHPPOTE network configuration. Defaults to:
 * - bind:        0.0.0.0:0
 * - broadcast:   255.255.255.255:60000
 * - listen:      0.0.0.0:60001
 * - timeout:     5s
 * - controllers: (none)
 * - debug:       false
 *
 * NOTES: 1. https://wiki.sei.cmu.edu/confluence/display/cplusplus/EXP58-CPP.+Pass+an+object+of+the+correct+type+to+va_start
 *        2. https://www.linkedin.com/pulse/modern-c-variadic-functions-how-shoot-yourself-foot-avoid-zinin
 */
void setup(const char *bind, const char *broadcast, const char *listen, int timeout, int debug, ...) {
    if (u != NULL) {
        teardown();
    }

    if ((u = (UHPPOTE *)malloc(sizeof(UHPPOTE))) != NULL) {
        u->bind = bind;
        u->broadcast = broadcast;
        u->listen = listen;
        u->timeout = timeout;
        u->devices = NULL;
        u->debug = debug != 0;

        udevices *devices;
        udevice *list;
        uint32_t N = 0;

        va_list args;
        {
            va_start(args, debug);
            controller *p = va_arg(args, controller *);
            while (p != NULL) {
                N++;
                p = va_arg(args, controller *);
            }
            va_end(args);
        }

        if ((devices = (udevices *)malloc(sizeof(udevices))) == NULL) {
            return;
        }

        if ((list = (udevice *)malloc(sizeof(udevice) * N)) == NULL) {
            free(devices);
            return;
        }

        {
            va_start(args, debug);
            controller *p = va_arg(args, controller *);
            int ix = 0;

            // NTS: strdup's address because the controllers may go out of scope after the invocation of setup(..)
            while (p != NULL) {
                list[ix].id = p->id;
                list[ix].address = strdup(p->address);
                ix++;
                p = va_arg(args, controller *);
            }
            va_end(args);
        }

        u->devices = devices;
        u->devices->N = N;
        u->devices->devices = list;
    }
}

void teardown() {
    if (u != NULL) {
        udevices *devices;

        if ((devices = u->devices) != NULL) {
            free(devices->devices);
            free(devices);
        }
    }

    if (err != NULL) {
        free(err);
    }
}

void set_error(char *errmsg) {
    unsigned l = strlen(errmsg) + 1;

    if (err != NULL) {
        free(err);
    }

    if ((err = malloc(l)) != NULL) {
        snprintf(err, l, "%s", errmsg);
    }

    free(errmsg);
}

int get_devices(uint32_t **devices, int *N) {
    uint32_t *list = NULL;
    int allocated = 0;

    for (;;) {
        allocated += 16;
        if ((list = realloc(list, allocated * sizeof(uint32_t))) == NULL) {
            free(list);
            set_error("Error allocating storage for slice");
            return -1;
        }

        int count = allocated;
        char *err = GetDevices(u, &count, list);
        if (err != NULL) {
            free(list);
            set_error(err);
            return -1;
        }

        if (count <= allocated) {
            *N = count;
            *devices = malloc(count * sizeof(uint32_t));

            memmove(*devices, list, count * sizeof(uint32_t));
            free(list);

            return 0;
        }
    }
}

int get_device(unsigned id, struct device *d) {
    struct Device device;

    char *err = GetDevice(u, id, &device);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    d->ID = device.ID;

    snprintf(d->address, sizeof(d->address), "%s", device.address);
    snprintf(d->subnet, sizeof(d->subnet), "%s", device.subnet);
    snprintf(d->gateway, sizeof(d->gateway), "%s", device.gateway);
    snprintf(d->MAC, sizeof(d->MAC), "%s", device.MAC);
    snprintf(d->version, sizeof(d->version), "%s", device.version);
    snprintf(d->date, sizeof(d->date), "%s", device.date);

    free(device.address);
    free(device.subnet);
    free(device.gateway);
    free(device.MAC);
    free(device.version);
    free(device.date);

    return 0;
}
