#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uhppoted.h"

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_NUMBER;
extern const uint32_t CARD_INDEX;
extern const uint32_t EVENT_INDEX;
extern const uint8_t DOOR;

extern bool passed(const char *);
extern bool failed(const char *);

bool getEventIndex() {
    const char *tag = "get-event-index";
    uint32_t expected = 47;
    uint32_t index;

    if (get_event_index(DEVICE_ID, &index) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (index != expected) {
        printf("%s: incorrect event index - expected:%u, got:%u\n", tag, expected, index);
        ok = false;
    }

    if (!ok) {
        return failed(tag);
    }

    return passed(tag);
}

bool setEventIndex() {
    const char *tag = "set-event-index";
    uint32_t index = EVENT_INDEX;

    if (set_event_index(DEVICE_ID, index) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    return passed(tag);
}

bool getEvent() {
    const char *tag = "set-event";
    event event;

    if (get_event(DEVICE_ID, EVENT_INDEX, &event) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (strcmp(event.timestamp, "2022-04-15 12:29:15") != 0) {
        printf("%s: incorrect event timestamp - expected:%s, got:%s\n", tag, "2022-01-01", event.timestamp);
        ok = false;
    }

    if (event.index != 51) {
        printf("%s: incorrect event index - expected:%u, got:%u\n", tag, 51, event.index);
        ok = false;
    }

    if (event.eventType != 6) {
        printf("%s: incorrect event type - expected:%u, got:%u\n", tag, 6, event.eventType);
        ok = false;
    }

    if (!event.granted) {
        printf("%s: incorrect event granted - expected:%u, got:%u\n", tag, 1, event.granted);
        ok = false;
    }

    if (event.door != 3) {
        printf("%s: incorrect event door - expected:%u, got:%u\n", tag, 3, event.door);
        ok = false;
    }

    if (event.direction != 1) {
        printf("%s: incorrect event direction - expected:%u, got:%u\n", tag, 1, event.direction);
        ok = false;
    }

    if (event.card != 8165538) {
        printf("%s: incorrect event card number - expected:%u, got:%u\n", tag, 8165538, event.card);
        ok = false;
    }

    if (event.reason != 21) {
        printf("%s: incorrect event reason - expected:%u, got:%u\n", tag, 21, event.reason);
        ok = false;
    }

    if (!ok) {
        return failed(tag);
    }

    return passed(tag);
}

bool recordSpecialEvents() {
    const char *tag = "record-special-events";

    if (record_special_events(DEVICE_ID, true) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    return passed(tag);
}
