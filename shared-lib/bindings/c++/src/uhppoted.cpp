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
                   const vector<controller> &controllers, bool debug) : uhppoted() {
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
    s.sysdatetime = status.sysdatetime;

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
        s.event.timestamp = status.event->timestamp;
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

string uhppoted::get_listener(uint32_t id) {
    char *listener;

    char *err = GetListener(u, &listener, id);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    string l = string(listener);

    free(listener);

    return l;
}

void uhppoted::set_listener(uint32_t id, std::string &listener) {
    char *err = SetListener(u, id, (char *)listener.c_str());

    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}

struct door_control uhppoted::get_door_control(uint32_t id, uint8_t door) {
    struct DoorControl control;

    char *err = GetDoorControl(u, &control, id, door);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    struct door_control d;

    d.mode = control.mode;
    d.delay = control.delay;

    return d;
}

void uhppoted::set_door_control(uint32_t id, uint8_t door, uint8_t mode, uint8_t delay) {
    char *err = SetDoorControl(u, id, door, mode, delay);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::open_door(uint32_t id, uint8_t door) {
    char *err = OpenDoor(u, id, door);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}

int uhppoted::get_cards(uint32_t id) {
    int N;

    char *err = GetCards(u, &N, id);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    return N;
}

card uhppoted::get_card(uint32_t id, uint32_t card_number) {
    Card card;

    vector<uint8_t> doors(4);

    card.doors = doors.data();

    char *err = GetCard(u, &card, id, card_number);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    struct card c;

    c.card_number = card.card_number;
    c.from = card.from;
    c.to = card.to;
    c.doors[0] = card.doors[0];
    c.doors[1] = card.doors[1];
    c.doors[2] = card.doors[2];
    c.doors[3] = card.doors[3];

    return c;
}

card uhppoted::get_card_by_index(uint32_t id, uint32_t index) {
    Card card;

    vector<uint8_t> doors(4);

    card.doors = doors.data();

    char *err = GetCardByIndex(u, &card, id, index);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    struct card c;

    c.card_number = card.card_number;
    c.from = card.from;
    c.to = card.to;
    c.doors[0] = card.doors[0];
    c.doors[1] = card.doors[1];
    c.doors[2] = card.doors[2];
    c.doors[3] = card.doors[3];

    return c;
}

void uhppoted::put_card(uint32_t id, uint32_t card_number, string from, string to, uint8_t doors[4]) {
    char *err = PutCard(u, id, card_number, (char *)from.c_str(), (char *)to.c_str(), (uint8_t *)doors);

    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::delete_card(uint32_t id, uint32_t card_number) {
    char *err = DeleteCard(u, id, card_number);

    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::delete_cards(uint32_t id) {
    char *err = DeleteCards(u, id);

    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}

uint32_t uhppoted::get_event_index(uint32_t id) {
    uint32_t index;

    char *err = GetEventIndex(u, &index, id);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    return index;
}

void uhppoted::set_event_index(uint32_t id, uint32_t index) {
    char *err = SetEventIndex(u, id, index);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}

event uhppoted::get_event(uint32_t id, uint32_t index) {
    Event event;

    char *err = GetEvent(u, &event, id, index);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    struct event e;

    e.timestamp = event.timestamp;
    e.index = event.index;
    e.eventType = event.eventType;
    e.granted = event.granted;
    e.door = event.door;
    e.direction = event.direction;
    e.card = event.card;
    e.reason = event.reason;

    return e;
}

void uhppoted::record_special_events(uint32_t id, bool enabled) {
    char *err = RecordSpecialEvents(u, id, enabled);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}

time_profile uhppoted::get_time_profile(uint32_t id, uint8_t profile_id) {
    TimeProfile profile;

    char *err = GetTimeProfile(u, &profile, id, profile_id);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }

    struct time_profile p;

    p.ID = profile.ID;
    p.linked = profile.linked;
    p.from = profile.from;
    p.to = profile.to;

    p.monday = profile.monday;
    p.tuesday = profile.tuesday;
    p.wednesday = profile.wednesday;
    p.thursday = profile.thursday;
    p.friday = profile.friday;
    p.saturday = profile.saturday;
    p.sunday = profile.sunday;

    p.segment1start = profile.segment1start;
    p.segment1end = profile.segment1end;
    p.segment2start = profile.segment2start;
    p.segment2end = profile.segment2end;
    p.segment3start = profile.segment3start;
    p.segment3end = profile.segment3end;

    return p;
}

void uhppoted::set_time_profile(uint32_t id, const time_profile &p) {
    TimeProfile profile;

    profile.ID = p.ID;
    profile.linked = p.linked;
    profile.from = (char *)p.from.c_str();
    profile.to = (char *)p.to.c_str();
    profile.monday = p.monday;
    profile.tuesday = p.tuesday;
    profile.wednesday = p.wednesday;
    profile.thursday = p.thursday;
    profile.friday = p.friday;
    profile.saturday = p.saturday;
    profile.sunday = p.sunday;
    profile.segment1start = (char *)p.segment1start.c_str();
    profile.segment1end = (char *)p.segment1end.c_str();
    profile.segment2start = (char *)p.segment2start.c_str();
    profile.segment2end = (char *)p.segment2end.c_str();
    profile.segment3start = (char *)p.segment3start.c_str();
    profile.segment3end = (char *)p.segment3end.c_str();

    char *err = SetTimeProfile(u, id, &profile);
    if (err != NULL) {
        throw uhppoted_exception(err);
    }
}
