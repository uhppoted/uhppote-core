#include <stdlib.h>
#include <iostream>

#include "../include/uhppote.hpp"

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
  
    if ((u = new UHPPOTE) != NULL) {
        u->bind = bind.c_str();
        u->broadcast = broadcast.c_str();
        u->listen = listen.c_str();
        u->timeout = timeout;
        u->devices = NULL;
        u->debug = debug;

        udevice *q = NULL;
        udevice *previous = NULL;
        for (auto p : controllers){
            if ((q = new udevice) != NULL) {
                // NTS: because the controllers may go out of scope  after the invocation of the
                //      constructor and c_str() returns a pointer to the underlying string char array
                size_t N    = p.address.size()+1;
                char  *addr = new char[N];
                p.address.copy(addr,N);

                q->id = p.id;
                q->address = addr; 
                q->next = previous;
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
            delete[] d->address;
            delete d;
            d = next;
        }        
    }

	delete u;
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
    std::vector<unsigned long> list;        
    int size = 0;

    do {
        size += 16;

        list.resize(size);
        GoSlice slice = { list.data(),size,size} ;

        rc = GetDevices(u, slice);
        if (rc.r1 != NULL) {
            set_error(rc.r1);
            return -1;
        }

    } while (rc.r0 > size);

    for (int i=0; i<rc.r0; i++) {
        devices.push_back(list[i]);
    }

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
