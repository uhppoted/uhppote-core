#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uhppoted.h"

extern const uint32_t DEVICE_ID;
extern const uint8_t DOOR;
extern bool result(char *test, bool ok);

bool getDevices() {
    uint32_t *devices = NULL;
    int N;

    if (get_devices(&devices, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (N != 3) {
        printf("get-devices: incorrect device count - expected:%u, got:%u\n", 3, N);
        ok = false;
    } else if (devices[0] != 201020304 || devices[1] != 303986753 || devices[2] != 405419896) {
        printf("get-devices: incorrect device list - expected:[%u,%u,%u], got:[%u,%u,%u]\n",
               201020304, 303986753, 405419896,
               devices[0], devices[1], devices[2]);
        ok = false;
    }

    free(devices);

    return result("get-devices", ok);
}

bool getDevice() {
    struct device d;

    if (get_device(DEVICE_ID, &d) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (d.ID != 405419896) {
        printf("get-device: incorrect device ID - expected:%u, got:%u\n", 405419896, d.ID);
        ok = false;
    }

    if (strcmp(d.address, "192.168.1.101") != 0) {
        printf("get-device: incorrect IP address - expected:%s, got:%s\n", "192.168.1.101", d.address);
        ok = false;
    }

    if (strcmp(d.subnet, "255.255.255.0") != 0) {
        printf("get-device: incorrect subnet mask - expected:%s, got:%s\n", "255.255.255.0", d.subnet);
        ok = false;
    }

    if (strcmp(d.gateway, "192.168.1.1") != 0) {
        printf("get-device: incorrect gateway address - expected:%s, got:%s\n", "192.168.1.1", d.gateway);
        ok = false;
    }

    if (strcmp(d.MAC, "00:12:23:34:45:56") != 0) {
        printf("get-device: incorrect MAC address - expected:%s, got:%s\n", "00:12:23:34:45:56", d.MAC);
        ok = false;
    }

    if (strcmp(d.version, "v8.92") != 0) {
        printf("get-device: incorrect version - expected:%s, got:%s\n", "v8.92", d.version);
        ok = false;
    }

    if (strcmp(d.date, "2018-11-05") != 0) {
        printf("get-device: incorrect date - expected:%s, got:%s\n", "2018-11-05", d.date);
        ok = false;
    }

    return result("get-device", ok);
}

bool setAddress(uint32_t deviceID, const char *address, const char *subnet,
                const char *gateway) {
    if (set_address(DEVICE_ID, "192.168.1.125", "255.255.254.0", "192.168.1.0") != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    return result("set-address", true);
}

bool getStatus() {
    struct status s;

    if (get_status(DEVICE_ID, &s) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (s.ID != DEVICE_ID) {
        printf("get-status: incorrect device ID - expected:%u, got:%u\n", DEVICE_ID, s.ID);
        ok = false;
    }

    if (strcmp(s.sysdatetime, "2022-03-19 15:48:32") != 0) {
        printf("get-status: incorrect system date/time - expected:%s, got:%s\n", "2022-03-19 15:48:32", s.sysdatetime);
        ok = false;
    }

    if (s.doors[0] != 1 || s.doors[1] != 0 || s.doors[2] != 0 || s.doors[3] != 1) {
        printf("get-status: incorrect doors state - expected:[%d,%d,%d,%d], got:[%d,%d,%d,%d]\n", 1, 0, 0, 1, s.doors[0], s.doors[1], s.doors[2],
               s.doors[3]);
        ok = false;
    }

    if (s.buttons[0] != 1 || s.buttons[1] != 0 || s.buttons[2] != 1 || s.buttons[3] != 0) {
        printf("get-status: incorrect buttons state - expected:[%d,%d,%d,%d], got:[%d,%d,%d,%d]\n", 1, 0, 0, 1, s.buttons[0], s.buttons[1], s.buttons[2],
               s.buttons[3]);
        ok = false;
    }

    if (s.relays != 0x12) {
        printf("get-status: incorrect relay state - expected:%u, got:%u\n", 0x12, s.relays);
        ok = false;
    }

    if (s.inputs != 0x34) {
        printf("get-status: incorrect inputs state - expected:%u, got:%u\n", 0x34, s.inputs);
        ok = false;
    }

    if (s.syserror != 0x56) {
        printf("get-status: incorrect system error - expected:%u, got:%u\n", 0x56, s.syserror);
        ok = false;
    }

    if (s.info != 253) {
        printf("get-status: incorrect special info - expected:%u, got:%u\n", 253, s.info);
        ok = false;
    }

    if (s.seqno != 9876) {
        printf("get-status: incorrect sequence number - expected:%u, got:%u\n", 9876, s.seqno);
        ok = false;
    }

    if (strcmp(s.event.timestamp, "2022-01-02 12:34:56") != 0) {
        printf("get-status: incorrect event timestamp - expected:%s, got:%s\n", "2022-01-02 12:34:56", s.event.timestamp);
        ok = false;
    }

    if (s.event.index != 135) {
        printf("get-status: incorrect event index - expected:%u, got:%u\n", 135, s.event.index);
        ok = false;
    }

    if (s.event.eventType != 6) {
        printf("get-status: incorrect event type - expected:%u, got:%u\n", 6, s.event.eventType);
        ok = false;
    }

    if (s.event.granted != 1) {
        printf("get-status: incorrect event granted - expected:%u, got:%u\n", 1, s.event.granted);
        ok = false;
    }

    if (s.event.door != 3) {
        printf("get-status: incorrect event door - expected:%u, got:%u\n", 3, s.event.door);
        ok = false;
    }

    if (s.event.direction != 1) {
        printf("get-status: incorrect event direction - expected:%u, got:%u\n", 1, s.event.direction);
        ok = false;
    }

    if (s.event.card != 8100023) {
        printf("get-status: incorrect event card - expected:%u, got:%u\n", 8100023, s.event.card);
        ok = false;
    }

    if (s.event.reason != 21) {
        printf("get-status: incorrect event reason - expected:%u, got:%u\n", 21, s.event.reason);
        ok = false;
    }

    return result("get-status", ok);
}

bool getTime() {
    char *datetime;

    if (get_time(DEVICE_ID, &datetime) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (strcmp(datetime, "2022-01-02 12:34:56") != 0) {
        printf("get-time: incorrect date/time - expected:%s, got:%s\n", "2022-01-02 12:34:56", datetime);
        ok = false;
    }

    free(datetime);

    return result("get-time", ok);
}

bool setTime() {
    if (set_time(DEVICE_ID, "2022-03-23 12:24:17") != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    return result("set-time", true);
}

bool getListener() {
    char *listener;

    if (get_listener(DEVICE_ID, &listener) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (strcmp(listener, "192.168.1.100:60001") != 0) {
        printf("get-listener: incorrect listener - expected:%s, got:%s\n", "192.168.1.100:60001", listener);
        ok = false;
    }

    free(listener);

    return result("get-listener", ok);
}

bool setListener() {
    if (set_listener(DEVICE_ID, "192.168.1.100:60001") != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    return result("set-listener", true);
}

bool getDoorControl() {
    struct door_control d;

    if (get_door_control(DEVICE_ID, DOOR, &d) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (d.mode != 3) {
        printf("get-door-control: incorrect door control mode - expected:%u, got:%u\n", 3, d.mode);
        ok = false;
    }

    if (d.delay != 7) {
        printf("get-door-control: incorrect door control delay - expected:%u, got:%u\n", 7, d.delay);
        ok = false;
    }

    return result("get-door-control", ok);
}

bool setDoorControl() {
    if (set_door_control(DEVICE_ID, DOOR, NORMALLY_CLOSED, 6) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    return result("set-door-control", true);
}
