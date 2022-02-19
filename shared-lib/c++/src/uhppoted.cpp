#include <stdlib.h>
#include <iostream>

#include "../include/uhppoted.hpp"

using namespace std;

uhppoted_exception::uhppoted_exception(char *err) {
   message = string(err);

   free(err); 
}

uhppoted_exception::~uhppoted_exception() {
}

const char * uhppoted_exception::what() const noexcept {
    return message.c_str();
}
  
uhppoted::uhppoted() {
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
uhppoted::uhppoted(const string& bind, const string& broadcast, const string& listen, int timeout, const vector<controller>& controllers, bool debug) : uhppoted() {
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

uhppoted::~uhppoted() {
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

vector<uint32_t> uhppoted::get_devices() {
    vector<uint32_t> list;        
    int allocated = 0;

    for (;;) {
        allocated += 16;
        list.resize(allocated);

        int count = allocated;
        char *err = GetDevices(u, &count, list.data());
        if (err != NULL) {
            throw uhppoted_exception(err);
        }

        if (count <= allocated) {
           vector<uint32_t> devices;
           for (int i=0; i<count; i++) {
               devices.push_back(list[i]);
           }

           return devices;            
        }
    } 
}

struct device uhppoted::get_device(uint32_t id) {
    struct GetDevice_return rc = GetDevice(u,id);

    if (rc.r1 != NULL) {
        throw uhppoted_exception(rc.r1);
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
