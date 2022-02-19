#include <stdlib.h>
#include <iostream>

#include "../include/uhppoted.hpp"

using namespace std;

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
uhppote::uhppote(const string& bind, const string& broadcast, const string& listen, int timeout, const vector<controller>& controllers, bool debug) {
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
                // NTS: because the controllers may go out of scope after the invocation of the
                //      constructor and c_str() returns a pointer to the underlying string char
                //      array
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

// All this finagling because you can't return a slice from Go
vector<uint32_t> uhppote::get_devices() {
    vector<uint32_t> list;        
    int size = 0;
    int count;

    do {
        size += 16;
        count = size;
        list.resize(size);

        char *err = GetDevices(u, &count, list.data());
        if (err != NULL) {
            throw runtime_error(err);
        }

    } while (count > size);

    vector<uint32_t> devices;
    for (int i=0; i<count; i++) {
        devices.push_back(list[i]);
    }

    return devices;
}

struct device uhppote::get_device(uint32_t id) {
    struct GetDevice_return rc = GetDevice(u,id);

    if (rc.r1 != NULL) {
        throw runtime_error(rc.r1);
    } else {
        device d;

        d.ID = rc.r0.ID;
        d.address = rc.r0.address;
        d.subnet = rc.r0.subnet;
        d.gateway = rc.r0.gateway;
        d.MAC = rc.r0.MAC;
        d.version = rc.r0.version;
        d.date = rc.r0.date;        

        return d;
    }
}
