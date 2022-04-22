#include "../include/uhppoted.h"
#include "libuhppoted.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

__thread char *err = NULL;
__thread UHPPOTE *u = NULL;

const char *errmsg() { return err; }

/* (optional) setup for UHPPOTE network configuration. Defaults to:
 * - bind:        0.0.0.0:0
 * - broadcast:   255.255.255.255:60000
 * - listen:      0.0.0.0:60001
 * - timeout:     5s
 * - controllers: (none)
 * - debug:       false
 *
 * NOTES: 1.
 * https://wiki.sei.cmu.edu/confluence/display/cplusplus/EXP58-CPP.+Pass+an+object+of+the+correct+type+to+va_start
 *        2.
 * https://www.linkedin.com/pulse/modern-c-variadic-functions-how-shoot-yourself-foot-avoid-zinin
 */
void setup(const char *bind, const char *broadcast, const char *listen,
           int timeout, int debug, ...) {
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

            // NTS: strdup's address because the controllers may go out of scope after
            // the invocation of setup(..)
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

int get_device(uint32_t id, struct device *d) {
    struct Device device;

    char *err = GetDevice(u, &device, id);
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

int set_address(uint32_t id, const char *address, const char *subnet,
                const char *gateway) {
    char *err =
        SetAddress(u, id, (char *)address, (char *)subnet, (char *)gateway);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}

int get_status(uint32_t id, struct status *s) {
    struct Status status;
    struct Event event;

    status.doors = malloc(4 * sizeof(uint8_t));
    status.buttons = malloc(4 * sizeof(uint8_t));
    status.event = &event;

    char *err = GetStatus(u, &status, id);
    if (err != NULL) {
        set_error(err);
        free(status.doors);
        free(status.buttons);
        return -1;
    }

    s->ID = status.ID;
    snprintf(s->sysdatetime, sizeof(s->sysdatetime), "%s", status.sysdatetime);

    s->doors[0] = status.doors[0];
    s->doors[1] = status.doors[1];
    s->doors[2] = status.doors[2];
    s->doors[3] = status.doors[3];

    s->buttons[0] = status.buttons[0];
    s->buttons[1] = status.buttons[1];
    s->buttons[2] = status.buttons[2];
    s->buttons[3] = status.buttons[3];

    s->relays = status.relays;
    s->inputs = status.inputs;
    s->syserror = status.syserror;
    s->info = status.info;
    s->seqno = status.seqno;

    if (status.event) {
        snprintf(s->event.timestamp, sizeof(s->event.timestamp), "%s", status.event->timestamp);
        s->event.index = status.event->index;
        s->event.eventType = status.event->eventType;
        s->event.granted = status.event->granted;
        s->event.door = status.event->door;
        s->event.direction = status.event->direction;
        s->event.card = status.event->card;
        s->event.reason = status.event->reason;
    }

    free(status.sysdatetime);
    free(status.doors);
    free(status.buttons);
    free(event.timestamp);

    return 0;
}

int get_time(uint32_t id, char **t) {
    char *datetime;

    char *err = GetTime(u, &datetime, id);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    *t = strdup(datetime);

    free(datetime);

    return 0;
}

int set_time(uint32_t id, char *datetime) {
    char *err = SetTime(u, id, datetime);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}

int get_listener(uint32_t id, char **t) {
    char *listener;

    char *err = GetListener(u, &listener, id);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    *t = strdup(listener);

    free(listener);

    return 0;
}

int set_listener(uint32_t id, char *listener) {
    char *err = SetListener(u, id, listener);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}

int get_door_control(uint32_t id, uint8_t door, struct door_control *c) {
    struct DoorControl control;

    char *err = GetDoorControl(u, &control, id, door);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    c->mode = control.mode;
    c->delay = control.delay;

    return 0;
}

int set_door_control(uint32_t id, uint8_t door, uint8_t mode, uint8_t delay) {
    char *err = SetDoorControl(u, id, door, mode, delay);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}

int open_door(uint32_t id, uint8_t door) {
    char *err = OpenDoor(u, id, door);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}

int get_cards(uint32_t id, int *N) {
    int cards;
    char *err = GetCards(u, &cards, id);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    *N = cards;

    return 0;
}

int get_card(uint32_t id, uint32_t card_number, card *c) {
    struct Card card;

    card.doors = malloc(4 * sizeof(uint8_t));

    char *err = GetCard(u, &card, id, card_number);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    c->card_number = card.card_number;

    snprintf(c->from, sizeof(c->from), "%s", card.from);
    snprintf(c->to, sizeof(c->to), "%s", card.to);

    c->doors[0] = card.doors[0];
    c->doors[1] = card.doors[1];
    c->doors[2] = card.doors[2];
    c->doors[3] = card.doors[3];

    free(card.from);
    free(card.to);
    free(card.doors);

    return 0;
}

int get_card_by_index(uint32_t id, uint32_t index, card *c) {
    struct Card card;

    card.doors = malloc(4 * sizeof(uint8_t));

    char *err = GetCardByIndex(u, &card, id, index);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    c->card_number = card.card_number;

    snprintf(c->from, sizeof(c->from), "%s", card.from);
    snprintf(c->to, sizeof(c->to), "%s", card.to);

    c->doors[0] = card.doors[0];
    c->doors[1] = card.doors[1];
    c->doors[2] = card.doors[2];
    c->doors[3] = card.doors[3];

    free(card.from);
    free(card.to);
    free(card.doors);

    return 0;
}

int put_card(uint32_t id, uint32_t card_number, const char *from, const char *to, const uint8_t doors[4]) {
    char *err = PutCard(u, id, card_number, (char *)from, (char *)to, (uint8_t *)doors);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}

int delete_card(uint32_t id, uint32_t card_number) {
    char *err = DeleteCard(u, id, card_number);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}

int delete_cards(uint32_t id) {
    char *err = DeleteCards(u, id);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}

int get_event_index(uint32_t id, uint32_t *index) {
    uint32_t ix;
    char *err = GetEventIndex(u, &ix, id);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    *index = ix;

    return 0;
}

int set_event_index(uint32_t id, uint32_t index) {
    char *err = SetEventIndex(u, id, index);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}

int get_event(uint32_t id, uint32_t index, event *e) {
    struct Event event;

    char *err = GetEvent(u, &event, id, index);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    snprintf(e->timestamp, sizeof(e->timestamp), "%s", event.timestamp);
    e->index = event.index;
    e->eventType = event.eventType;
    e->granted = event.granted;
    e->door = event.door;
    e->direction = event.direction;
    e->card = event.card;
    e->reason = event.reason;

    free(event.timestamp);

    return 0;
}

int record_special_events(uint32_t id, bool enabled) {
    char *err = RecordSpecialEvents(u, id, enabled);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}

int get_time_profile(uint32_t id, uint8_t profile_id, time_profile *p) {
    struct TimeProfile profile;

    char *err = GetTimeProfile(u, &profile, id, profile_id);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    p->ID = profile.ID;
    p->linked = profile.linked;

    snprintf(p->from, sizeof(p->from), "%s", profile.from);
    snprintf(p->to, sizeof(p->to), "%s", profile.to);

    p->monday = profile.monday;
    p->tuesday = profile.tuesday;
    p->wednesday = profile.wednesday;
    p->thursday = profile.thursday;
    p->friday = profile.friday;
    p->saturday = profile.saturday;
    p->sunday = profile.sunday;

    snprintf(p->segment1start, sizeof(p->segment1start), "%s", profile.segment1start);
    snprintf(p->segment1end, sizeof(p->segment1end), "%s", profile.segment1end);
    snprintf(p->segment2start, sizeof(p->segment2start), "%s", profile.segment2start);
    snprintf(p->segment2end, sizeof(p->segment2end), "%s", profile.segment2end);
    snprintf(p->segment3start, sizeof(p->segment3start), "%s", profile.segment3start);
    snprintf(p->segment3end, sizeof(p->segment3end), "%s", profile.segment3end);

    free(profile.from);
    free(profile.to);
    free(profile.segment1start);
    free(profile.segment1end);
    free(profile.segment2start);
    free(profile.segment2end);
    free(profile.segment3start);
    free(profile.segment3end);

    return 0;
}

int set_time_profile(uint32_t id, time_profile *p) {
    struct TimeProfile profile;

    profile.ID = p->ID;
    profile.linked = p->linked;
    profile.from = p->from;
    profile.to = p->to;
    profile.monday = p->monday;
    profile.tuesday = p->tuesday;
    profile.wednesday = p->wednesday;
    profile.thursday = p->thursday;
    profile.friday = p->friday;
    profile.saturday = p->saturday;
    profile.sunday = p->sunday;
    profile.segment1start = p->segment1start;
    profile.segment1end = p->segment1end;
    profile.segment2start = p->segment2start;
    profile.segment2end = p->segment2end;
    profile.segment3start = p->segment3start;
    profile.segment3end = p->segment3end;

    char *err = SetTimeProfile(u, id, &profile);
    if (err != NULL) {
        set_error(err);
        return -1;
    }

    return 0;
}
