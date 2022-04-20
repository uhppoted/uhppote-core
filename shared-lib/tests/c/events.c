#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tests.h"
#include "uhppoted.h"

bool getEventIndex() {
    uint32_t index;

    if (get_event_index(DEVICE_ID, &index) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {
        {
            .field = "event index",
            .type = "uint32",
            .value.uint32.expected = 47,
            .value.uint32.value = index,
        },
    };

    return evaluate("get-event-index", sizeof(resultset) / sizeof(result), resultset);
}

bool setEventIndex() {
    const char *tag = "set-event-index";

    if (set_event_index(DEVICE_ID, EVENT_INDEX) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool getEvent() {
    const char *tag = "set-event";
    event event;

    if (get_event(DEVICE_ID, EVENT_INDEX, &event) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    // clang-format off
    const result resultset[] = {
        { .field = "event timestamp", .type = "string",  .value.string.expected = "2022-04-15 12:29:15", .value.string.value = event.timestamp },
        { .field = "event index",     .type = "uint32",  .value.uint32.expected = 51,                    .value.uint32.value = event.index },
        { .field = "event type",      .type = "uint8",   .value.uint8.expected = 6,                      .value.uint8.value = event.eventType },
        { .field = "event granted",   .type = "boolean", .value.boolean.expected = true,                 .value.boolean.value = event.granted },
        { .field = "event door",      .type = "uint8",   .value.uint8.expected = 3,                      .value.uint8.value = event.door },
        { .field = "event direction", .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = event.direction },
        { .field = "event card",      .type = "uint32",  .value.uint32.expected = 8165538,               .value.uint32.value = event.card },
        { .field = "event reason",    .type = "uint8",   .value.uint8.expected = 21,                     .value.uint8.value = event.reason },
    };
    // clang-format on

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool recordSpecialEvents() {
    const char *tag = "record-special-events";

    if (record_special_events(DEVICE_ID, true) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}
