#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uhppoted.h"

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_NUMBER;
extern const uint32_t CARD_INDEX;
extern const uint8_t DOOR;
extern bool result(char *test, bool ok);

bool getEventIndex() {
    int index;

    if (get_event_index(DEVICE_ID, &index) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (index != 47) {
        printf("get-event-index: incorrect event index - expected:%u, got:%u\n", 47, index);
        ok = false;
    }

    return result("get-event-index", ok);
}
