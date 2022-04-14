#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uhppoted.h"

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_NUMBER;
extern const uint32_t CARD_INDEX;
extern const uint8_t DOOR;
extern bool result(const char *, bool);

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

    return result(tag, ok);
}

bool setEventIndex() {
    const char *tag = "set-event-index";
    uint32_t index = 51;

    if (set_event_index(DEVICE_ID, index) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    return result(tag, true);
}
