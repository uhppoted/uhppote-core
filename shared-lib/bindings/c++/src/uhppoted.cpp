#include <iostream>
#include <stdlib.h>

#include "../include/uhppoted.hpp"

using namespace std;

// NTS: std::make_shared can throw an exception but there doesn't seem
//      to be a clean alternative
uhppoted_exception::uhppoted_exception(char *err) {
    message = make_shared<char *>(err);
}

uhppoted_exception::~uhppoted_exception() {}

const char *uhppoted_exception::what() const noexcept { return *message; }

uhppoted::uhppoted() { u = NULL; }

/* (optional) setup for UHPPOTE network configuration. Defaults to:
 * - bind:        0.0.0.0:0
 * - broadcast:   255.255.255.255:60000
 * - listen:      0.0.0.0:60001
 * - timeout:     5s
 * - controllers: (none)
 * - debug:       false
 *
 */
uhppoted::uhppoted(const string &bind, const string &broadcast,
                   const string &listen, int timeout,
                   const vector<controller> &controllers, bool debug)
    : uhppoted() {
    if ((u = new UHPPOTE) != NULL) {
        u->bind = bind.c_str();
        u->broadcast = broadcast.c_str();
        u->listen = listen.c_str();
        u->timeout = timeout;
        u->devices = NULL;
        u->debug = debug;

        udevices *devices;
        udevice *list;
        uint32_t N = controllers.size();
        int ix = 0;

        if ((devices = new udevices) == NULL) {
            return;
        }

        if ((list = new udevice[N]) == NULL) {
            delete devices;
            return;
        }

        for (auto p : controllers) {
            // NTS: because the controllers may go out of scope after the invocation
            // of the
            //      constructor and c_str() returns a pointer to the underlying string
            //      char array
            size_t N = p.address.size() + 1;
            char *addr = new char[N];
            p.address.copy(addr, N);

            list[ix].id = p.id;
            list[ix].address = addr;
            ix++;
        }

        u->devices = devices;
        u->devices->N = N;
        u->devices->devices = list;
    }
}

uhppoted::~uhppoted() {
    if (u != NULL) {
        udevices *devices;

        if ((devices = u->devices) != NULL) {
            delete[] devices->devices;
            delete devices;
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

    char *err = GetDevice(u, &device, id);
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

void uhppoted::set_address(uint32_t id, std::string &address,
                           std::string &subnet, std::string &gateway) {
    char *err = SetAddress(u, id, (char *)address.c_str(), (char *)subnet.c_str(),
                           (char *)gateway.c_str());
    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}

status uhppoted::get_status(unsigned id) {
    struct Status status;
    struct Event event;
    vector<uint8_t> doors(4);
    vector<uint8_t> buttons(4);

    status.doors = doors.data();
    status.buttons = buttons.data();
    status.event = &event;

    char *err = GetStatus(u, &status, id);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    struct status s;

    s.ID = status.ID;
    snprintf(s.sysdatetime, sizeof(s.sysdatetime), "%s", status.sysdatetime);

    s.doors[0] = doors[0];
    s.doors[1] = doors[1];
    s.doors[2] = doors[2];
    s.doors[3] = doors[3];

    s.buttons[0] = buttons[0];
    s.buttons[1] = buttons[1];
    s.buttons[2] = buttons[2];
    s.buttons[3] = buttons[3];

    s.relays = status.relays;
    s.inputs = status.inputs;
    s.syserror = status.syserror;
    s.seqno = status.seqno;
    s.info = status.info;

    if (status.event) {
        snprintf(s.event.timestamp, sizeof(s.event.timestamp), "%s",
                 status.event->timestamp);
        s.event.index = status.event->index;
        s.event.eventType = status.event->eventType;
        s.event.granted = status.event->granted;
        s.event.door = status.event->door;
        s.event.direction = status.event->direction;
        s.event.card = status.event->card;
        s.event.reason = status.event->reason;
    }

    free(status.sysdatetime);
    free(event.timestamp);

    return s;
}

string uhppoted::get_time(uint32_t id) {
    char *datetime;

    char *err = GetTime(u, &datetime, id);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    string t = string(datetime);

    free(datetime);

    return t;
}

void uhppoted::set_time(uint32_t id, std::string &datetime) {
    char *err = SetTime(u, id, (char *)datetime.c_str());

    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}
