#include "../include/uhppote.hpp"

#include <stdlib.h>
#include <iostream>

uhppote::uhppote() {
    u = NULL;
}

/* (optional) setup for UHPPOTE network configuration. Defaults to:
 * - bind:        0.0.0.0:0
 * - broadcast:   255.255.255.255:60000
 * - listen:      0.0.0.0:60001
 * - timeout:     5s
 * - controllers: (none)
 * - debug:       false
 *
 */ 
uhppote::uhppote(const std::string& bind, const std::string& broadcast, const std::string& listen, int timeout, const std::vector<controller>& controllers, bool debug) {
    uhppote();

    if ((u = (UHPPOTE *) malloc(sizeof(UHPPOTE))) != NULL) {
        u->bind = bind.c_str();
        u->broadcast = broadcast.c_str();
        u->listen = listen.c_str();
        u->timeout = timeout;
        u->devices = NULL;
        u->debug = debug;

        udevice *q = NULL;
        udevice *previous = NULL;
        for (auto p : controllers){
            if ((q = (udevice *) malloc(sizeof(udevice))) != NULL) {
                q->id = p.id;
                q->address = strdup(p.address.c_str()); // C.GoString doesn't seem to respect the const'ness
                q->next=previous;
                previous = q;
            }
        }

		u->devices = q;
    }
}

uhppote::~uhppote() {
    if (u != NULL) {
        udevice *d = u->devices;

        while (d != NULL) {
            udevice *next = d->next;
            free(d->address);
            free(d);
            d = next;
        }        
    }

    free(u);
}

std::string uhppote::errmsg() {
    return err;
}

void uhppote::set_error(const char *errmsg) {
	err = errmsg;
}

// All this finagling because you can't return a slice from Go
int uhppote::get_devices(std::vector<unsigned long>& devices) {
    struct GetDevices_return rc;
    unsigned long *list = NULL;        
    int size = 0;

    do {
        size += 16;

        unsigned long *p;        
        if ((p = (unsigned long *) realloc(list, size * sizeof(unsigned long))) == NULL) {
            free(list);
            set_error("Error allocating storage for slice");
            return -1;            
        } else {
            list = p;
        }

        GoSlice slice = { list,size,size} ;

        rc = GetDevices(u, slice);
        if (rc.r1 != NULL) {
            free(list);
            set_error(rc.r1);
            return -1;
        }

    } while (rc.r0 > size);

    for (int i=0; i<rc.r0; i++) {
        devices.push_back(list[i]);
    }

    free(list);

    return 0;
}

int uhppote::get_device(unsigned id, struct device& d) {
    struct GetDevice_return rc = GetDevice(u,id);

    if (rc.r1 != NULL) {        
        set_error(rc.r1);
        return -1;
    }

    d.ID = rc.r0.ID;
    d.address = rc.r0.address;
    d.subnet = rc.r0.subnet;
    d.gateway = rc.r0.gateway;
    d.MAC = rc.r0.MAC;
    d.version = rc.r0.version;
    d.date = rc.r0.date;

    return 0;
}
