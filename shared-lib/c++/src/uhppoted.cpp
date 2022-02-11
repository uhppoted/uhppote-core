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
        u->devices.N = 0;
        u->devices.devices = NULL;
        u->debug = debug;

        vector<udevice *> devices;
        for (auto p : controllers){
            auto q = new udevice;
            if (q != NULL) {
                // NTS: because the controllers may go out of scope after the invocation of the
                //      constructor and c_str() returns a pointer to the underlying string char
                //      array
                size_t N    = p.address.size()+1;
                char  *addr = new char[N];
                p.address.copy(addr,N);

                q->id = p.id;
                q->address = addr; 

                devices.push_back(q);
            }
        }

        u->devices.N = devices.size();
        u->devices.devices = new udevice *[devices.size()];

        copy(devices.begin(), devices.end(), u->devices.devices);
    }
}

uhppote::~uhppote() {
    if (u != NULL) {
        udevice *d;

        for (int i=0; i<u->devices.N; i++) {
            if ((d = u->devices.devices[i]) != NULL) {
                delete[] d->address;                
            }
            
            delete d;
        }

        delete[] u->devices.devices;
    }

	delete u;
}

// All this finagling because you can't return a slice from Go
vector<uint32_t> uhppote::get_devices() {
    struct GetDevices_return rc;
    vector<uint32_t> list;        
    int size = 0;

    do {
        size += 16;

        list.resize(size);
        GoSlice slice = { list.data(),size,size} ;

        rc = GetDevices(u, slice);
        if (rc.r1 != NULL) {
            throw runtime_error(rc.r1);
        }

    } while (rc.r0 > size);

    vector<uint32_t> devices;
    for (int i=0; i<rc.r0; i++) {
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
