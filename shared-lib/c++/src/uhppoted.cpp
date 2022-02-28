#include <iostream>
#include <stdlib.h>

#include "../include/uhppoted.hpp"

using namespace std;

// NTS: std::make_shared can throw an exception but there doesn't seem
//      to be a clean alternative
uhppoted_exception::uhppoted_exception(char *err) {
    message = make_shared<char *>(err);
}

uhppoted_exception::~uhppoted_exception() {
}

const char *uhppoted_exception::what() const noexcept {
    return *message;
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
uhppoted::uhppoted(const string &bind, const string &broadcast, const string &listen, int timeout, const vector<controller> &controllers, bool debug) : uhppoted() {
    if ((u = new UHPPOTE) != NULL) {
        u->bind = bind.c_str();
        u->broadcast = broadcast.c_str();
        u->listen = listen.c_str();
        u->timeout = timeout;
        u->devices = NULL;
        u->debug = debug;

        udevice *q = NULL;
        udevice *previous = NULL;
        for (auto p : controllers) {
            if ((q = new udevice) != NULL) {
                // NTS: because the controllers may go out of scope after the invocation of the
                //      constructor and c_str() returns a pointer to the underlying string char
                //      array
                size_t N = p.address.size() + 1;
                char *addr = new char[N];
                p.address.copy(addr, N);

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
            for (int i = 0; i < count; i++) {
                devices.push_back(list[i]);
            }

            return devices;
        }
    }
}

struct device uhppoted::get_device(uint32_t id) {
    struct Device device;

    char *err = GetDevice(u, id, &device);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    struct device d;

    d.ID = device.ID;
    d.address = device.address;
    d.subnet = device.subnet;
    d.gateway = device.gateway;
    d.MAC = device.MAC;
    d.version = device.version;
    d.date = device.date;

    free(device.address);
    free(device.subnet);
    free(device.gateway);
    free(device.MAC);
    free(device.version);
    free(device.date);

    return d;
}
