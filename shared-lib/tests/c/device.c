#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "device.h"
#include "uhppoted.h"

const uint32_t DEVICEID = 405419896;

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

    if (ok) {
        printf("get-devices: ok\n");
    }

    return ok;
}

bool getDevice() {
    struct device d;

    if (get_device(DEVICEID, &d) != 0) {
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

    if (ok) {
        printf("get-device:  ok\n");
    }

    return ok;
}

bool setAddress(uint32_t deviceID, const char *address, const char *subnet,
                const char *gateway) {
    if (set_address(DEVICEID, "192.168.1.125", "255.255.254.0", "192.168.1.0") != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    printf("set-address: ok\n");

    return true;
}

bool getStatus() {
    struct status s;

    if (get_status(DEVICEID, &s) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (s.ID != DEVICEID) {
        printf("get-status: incorrect device ID - expected:%u, got:%u\n", DEVICEID, s.ID);
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

    if (ok) {
        printf("get-status:  ok\n");
    }

    return ok;
}

bool getTime() {
    char *datetime;

    if (get_time(DEVICEID, &datetime) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (strcmp(datetime, "2022-01-02 12:34:56") != 0) {
        printf("get-time: incorrect date/time - expected:%s, got:%s\n", "2022-01-02 12:34:56", datetime);
        ok = false;
    }

    if (ok) {
        printf("get-time:    ok\n");
    }

    free(datetime);

    return ok;
}

bool setTime() {
    bool ok = true;

    if (set_time(DEVICEID, "2022-03-23 12:24:17") != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    if (ok) {
        printf("set-time:    ok\n");
    }

    return ok;
}
