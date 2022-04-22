#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tests.h"
#include "uhppoted.h"

void usage();
bool all();

typedef bool (*f)();

typedef struct test {
    const char *name;
    f fn;
} test;

bool passed(const char *);
bool failed(const char *);

const uint32_t DEVICE_ID = 405419896;
const uint32_t CARD_NUMBER = 8165538;
const uint32_t CARD_INDEX = 19;
const uint32_t EVENT_INDEX = 51;
const uint8_t DOOR = 4;
const uint8_t PROFILE_ID = 49;

const test tests[] = {
    {.name = "get-devices", .fn = getDevices},
    {.name = "get-device", .fn = getDevice},
    {.name = "set-address", .fn = setAddress},
    {.name = "get-status", .fn = getStatus},
    {.name = "get-time", .fn = getTime},
    {.name = "set-time", .fn = setTime},
    {.name = "get-listener", .fn = getListener},
    {.name = "set-listener", .fn = setListener},
    {.name = "get-door-control", .fn = getDoorControl},
    {.name = "set-door-control", .fn = setDoorControl},
    {.name = "open-door", .fn = openDoor},
    {.name = "get-cards", .fn = getCards},
    {.name = "get-card", .fn = getCard},
    {.name = "get-card-by-index", .fn = getCardByIndex},
    {.name = "put-card", .fn = putCard},
    {.name = "delete-card", .fn = deleteCard},
    {.name = "delete-cards", .fn = deleteCards},
    {.name = "get-event-index", .fn = getEventIndex},
    {.name = "set-event-index", .fn = setEventIndex},
    {.name = "get-event", .fn = getEvent},
    {.name = "record-special-events", .fn = recordSpecialEvents},
    {.name = "get-time-profile", .fn = getTimeProfile},
    {.name = "set-time-profile", .fn = setTimeProfile},
};

controller alpha = {.id = 405419896, .address = "192.168.1.100"};
controller beta = {.id = 303986753, .address = "192.168.1.100"};

int main(int argc, char **argv) {
    bool ok = true;
    char *cmd;

    if (argc > 1) {
        cmd = argv[1];
    }

    setup("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2500, true, &alpha, &beta, NULL);

    if (cmd == NULL || strncmp(cmd, "all", 3) == 0) {
        ok = all();
    } else if (strcmp(cmd, "help") == 0) {
        printf("\n");
        usage();
    } else {
        int N = sizeof(tests) / sizeof(test);

        for (int i = 0; i < N; i++) {
            test t = tests[i];
            if (strcmp(cmd, t.name) == 0) {
                ok = t.fn();
                goto done; // <evil cackle> always wanted to do this just to annoy somebody on the Internet
            }
        }

        printf("\n*** ERROR invalid command (%s)\n\n", cmd);
        usage();
        ok = false;
    }

done:
    teardown();

    return ok ? 0 : -1;
}

bool all() {
    bool ok = true;
    int N = sizeof(tests) / sizeof(test);

    for (int i = 0; i < N; i++) {
        test t = tests[i];
        ok = t.fn() ? ok : false;
    }

    return ok;
}

void usage() {
    int N = sizeof(tests) / sizeof(test);

    printf("   Usage: test <command>\n");
    printf("\n");
    printf("   Supported commands:\n");

    for (int i = 0; i < N; i++) {
        test t = tests[i];
        printf("      %s\n", t.name);
    }

    printf("\n");
}

bool evaluate(const char *tag, int N, const result resultset[]) {
    bool ok = true;

    for (int i = 0; i < N; i++) {
        result r = resultset[i];
        if (strcmp(r.type, "uint8") == 0) {
            if (r.value.uint8.value != r.value.uint8.expected) {
                printf("%-21s incorrect %s (expected:%u, got:%u)\n", tag, r.field, r.value.uint8.expected, r.value.uint8.value);
                ok = false;
            }
        } else if (strcmp(r.type, "uint32") == 0) {
            if (r.value.uint32.value != r.value.uint32.expected) {
                printf("%-21s incorrect %s (expected:%u, got:%u)\n", tag, r.field, r.value.uint32.expected, r.value.uint32.value);
                ok = false;
            }
        } else if (strcmp(r.type, "boolean") == 0) {
            if (r.value.boolean.value != r.value.boolean.expected) {
                printf("%-21s incorrect %s (expected:%d, got:%d)\n", tag, r.field, r.value.boolean.expected, r.value.boolean.value);
                ok = false;
            }
        } else if (strcmp(r.type, "string") == 0) {
            if (strcmp(r.value.string.value, r.value.string.expected) != 0) {
                printf("%-21s incorrect %s (expected:%s, got:%s)\n", tag, r.field, r.value.string.expected, r.value.string.value);
                ok = false;
            }
        } else {
            printf("invalid result type: field::%s,type:%s\n", r.field, r.type);
            return false;
        }
    }

    if (!ok) {
        return failed(tag);
    }

    return passed(tag);
}

bool passed(const char *tag) {
    printf("%-21s ok\n", tag);

    return true;
}

bool failed(const char *tag) {
    printf("%-21s failed\n", tag);

    return false;
}
