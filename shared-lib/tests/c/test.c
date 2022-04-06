#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cards.h"
#include "device.h"
#include "uhppoted.h"

void usage();
bool all();

typedef bool (*f)();

typedef struct test {
    const char *name;
    f fn;
} test;

const uint32_t DEVICEID = 405419896;
const uint32_t CARDID = 8165538;
const uint8_t DOOR = 4;

bool result(char *test, bool ok) {
    if (ok) {
        printf("%-16s  ok\n", test);
    }

    return ok;
}

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
    {.name = "get-cards", .fn = getCards},
    {.name = "get-card", .fn = getCard},
};

int main(int argc, char **argv) {
    bool ok = true;
    char *cmd;

    if (argc > 1) {
        cmd = argv[1];
    }

    controller alpha = {.id = 405419896, .address = "192.168.1.100"};
    controller beta = {.id = 303986753, .address = "192.168.1.100"};

    setup("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2500, true, &alpha, &beta, NULL);

    if (cmd == NULL || strncmp(cmd, "all", 3) == 0) {
        ok = all();
    } else {
        int N = sizeof(tests) / sizeof(test);

        for (int i = 0; i < N; i++) {
            test t = tests[i];
            if (strncmp(cmd, t.name, strlen(t.name)) == 0) {
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
    printf("      all\n");

    for (int i = 0; i < N; i++) {
        test t = tests[i];
        printf("      %s\n", t.name);
    }

    printf("\n");
}
