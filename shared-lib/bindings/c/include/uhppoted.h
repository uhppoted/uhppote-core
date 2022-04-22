#pragma once

#include <stdbool.h>
#include <stdint.h>

#define NORMALLY_OPEN 1
#define NORMALLY_CLOSED 2
#define CONTROLLED 3

typedef struct controller {
    uint32_t id;
    const char *address;
} controller;

typedef struct device {
    uint32_t ID;
    char address[16];
    char subnet[16];
    char gateway[16];
    char MAC[18];
    char version[6];
    char date[11];
} device;

typedef struct event {
    char timestamp[20];
    uint32_t index;
    uint8_t eventType;
    bool granted;
    uint8_t door;
    uint8_t direction;
    uint32_t card;
    uint8_t reason;
} event;

typedef struct status {
    uint32_t ID;
    char sysdatetime[20];
    bool doors[4];
    bool buttons[4];
    uint8_t relays;
    uint8_t inputs;
    uint8_t syserror;
    uint8_t info;
    uint32_t seqno;
    event event;
} status;

typedef struct door_control {
    uint8_t mode;
    uint8_t delay;
} door_control;

typedef struct card {
    uint32_t card_number;
    char from[11];
    char to[11];
    uint8_t doors[4];
} card;

typedef struct time_profile {
    uint8_t ID;
    uint8_t linked;
    char from[11];
    char to[11];
    bool monday;
    bool tuesday;
    bool wednesday;
    bool thursday;
    bool friday;
    bool saturday;
    bool sunday;
    char segment1start[6];
    char segment1end[6];
    char segment2start[6];
    char segment2end[6];
    char segment3start[6];
    char segment3end[6];
} time_profile;

void setup(const char *bind, const char *broadcast, const char *listen, int timeout, int debug, ...);
void teardown();
const char *errmsg();

int get_devices(uint32_t **devices, int *N);
int get_device(uint32_t id, struct device *);
int set_address(uint32_t id, const char *address, const char *subnet,
                const char *gateway);
int get_status(uint32_t id, struct status *);
int get_time(uint32_t id, char **);
int set_time(uint32_t id, char *);
int get_listener(uint32_t id, char **);
int set_listener(uint32_t id, char *);
int get_door_control(uint32_t id, uint8_t door, struct door_control *);
int set_door_control(uint32_t id, uint8_t door, uint8_t mode, uint8_t delay);
int open_door(uint32_t id, uint8_t door);

int get_cards(uint32_t id, int *N);
int get_card(uint32_t id, uint32_t card_number, card *card);
int get_card_by_index(uint32_t id, uint32_t index, card *card);
int put_card(uint32_t id, uint32_t card_number, const char *from, const char *to, const uint8_t doors[4]);
int delete_card(uint32_t id, uint32_t card_number);
int delete_cards(uint32_t id);

int get_event_index(uint32_t id, uint32_t *index);
int set_event_index(uint32_t id, uint32_t index);
int get_event(uint32_t id, uint32_t index, event *event);
int record_special_events(uint32_t id, bool enabled);

int get_time_profile(uint32_t id, uint8_t profile_id, time_profile *profile);
int set_time_profile(uint32_t id, time_profile *profile);
