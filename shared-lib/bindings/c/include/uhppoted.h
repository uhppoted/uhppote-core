#pragma once

#include <stdbool.h>
#include <stdint.h>

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
    uint8_t control;
    uint8_t delay;
} door_control;

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
