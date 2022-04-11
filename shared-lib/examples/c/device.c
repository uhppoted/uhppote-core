#include <stdio.h>
#include <stdlib.h>

#include "device.h"
#include "uhppoted.h"

extern uint32_t DEVICE_ID;

int getDevices(int argc, char **argv) {
    uint32_t *devices = NULL;
    int N;

    if (get_devices(&devices, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-devices (%d)\n", N);
    if (N > 0 && devices != NULL) {
        for (int i = 0; i < N; i++) {
            printf("   %u\n", devices[i]);
        }
    }
    printf("\n");

    free(devices);

    return 0;
}

int getDevice(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    struct device d;

    if (get_device(deviceID, &d) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-device\n");
    printf("  ID:       %u\n", d.ID);
    printf("  IP:       %s  %s  %s\n", d.address, d.subnet, d.gateway);
    printf("  MAC:      %s\n", d.MAC);
    printf("  version:  %s\n", d.version);
    printf("  released: %s\n", d.date);
    printf("\n");

    return 0;
}

int setAddress(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    const char *address = "192.168.1.125";
    const char *subnet = "255.255.254.0";
    const char *gateway = "192.168.1.0";

    if (set_address(deviceID, address, subnet, gateway) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    return 0;
}

int getStatus(uint32_t deviceID) {
    struct status s;

    if (get_status(deviceID, &s) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-status\n");
    printf("  ID:        %u\n", s.ID);
    printf("  date/time: %s\n", s.sysdatetime);
    printf("  doors:     %d %d %d %d\n", s.doors[0], s.doors[1], s.doors[2],
           s.doors[3]);
    printf("  buttons:   %d %d %d %d\n", s.buttons[0], s.buttons[1], s.buttons[2],
           s.buttons[3]);
    printf("  relays:    %02X\n", s.relays);
    printf("  inputs:    %02X\n", s.inputs);
    printf("  error:     %02X\n", s.syserror);
    printf("  seq no.:   %u\n", s.seqno);
    printf("  info:      %u\n", s.info);
    printf("\n");
    printf("  event timestamp: %s\n", s.event.timestamp);
    printf("        index:     %u\n", s.event.index);
    printf("        type:      %u\n", s.event.eventType);
    printf("        granted:   %d\n", s.event.granted);
    printf("        door:      %d\n", s.event.door);
    printf("        direction: %d\n", s.event.direction);
    printf("        card:      %u\n", s.event.card);
    printf("        reason:    %d\n", s.event.reason);
    printf("\n");

    return 0;
}

int getTime(uint32_t deviceID) {
    char *datetime;

    if (get_time(deviceID, &datetime) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-time\n");
    printf("  ID:        %u\n", deviceID);
    printf("  date/time: %s\n", datetime);
    printf("\n");

    free(datetime);

    return 0;
}

int setTime(uint32_t deviceID, const char *datetime) {
    if (set_time(deviceID, (char *)datetime) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nset-time\n");
    printf("  ID:        %u\n", deviceID);
    printf("  date/time: %s\n", datetime);
    printf("\n");

    return 0;
}

int getListener(uint32_t deviceID) {
    char *listener;

    if (get_listener(deviceID, &listener) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-listener\n");
    printf("  ID:       %u\n", deviceID);
    printf("  listener: %s\n", listener);
    printf("\n");

    free(listener);

    return 0;
}

int setListener(uint32_t deviceID, const char *listener) {
    if (set_listener(deviceID, (char *)listener) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nset-listener\n");
    printf("  ID:             %u\n", deviceID);
    printf("  event listener: %s\n", listener);
    printf("\n");

    return 0;
}

int getDoorControl(uint32_t deviceID, uint8_t door) {
    struct door_control control;

    if (get_door_control(deviceID, door, &control) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-door-control\n");
    printf("  ID:    %u\n", deviceID);
    printf("  door:  %u\n", door);

    switch (control.mode) {
    case NORMALLY_OPEN:
        printf("  mode:  %s\n", "normally open");
        break;

    case NORMALLY_CLOSED:
        printf("  mode:  %s\n", "normally closed");
        break;

    case CONTROLLED:
        printf("  mode:  %s\n", "controlled");
        break;

    default:
        printf("  mode:  %s\n", "???");
    }

    printf("  delay: %us\n", control.delay);
    printf("\n");

    return 0;
}

int setDoorControl(uint32_t deviceID, uint8_t door, uint8_t mode, uint8_t delay) {
    if (set_door_control(deviceID, door, mode, delay) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nset-door-control\n");
    printf("  ID:    %u\n", deviceID);
    printf("  door:  %u\n", door);

    switch (mode) {
    case NORMALLY_OPEN:
        printf("  mode:  %s\n", "normally open");
        break;

    case NORMALLY_CLOSED:
        printf("  mode:  %s\n", "normally closed");
        break;

    case CONTROLLED:
        printf("  mode:  %s\n", "controlled");
        break;

    default:
        printf("  mode:  %s\n", "???");
    }

    printf("  delay: %us\n", delay);
    printf("\n");

    return 0;
}
