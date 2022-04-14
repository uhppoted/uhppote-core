#include <stdio.h>
#include <stdlib.h>

#include "uhppoted.h"

extern uint32_t DEVICE_ID;
extern uint32_t CARD_NUMBER;
extern uint32_t CARD_INDEX;
extern uint32_t EVENT_INDEX;

int getEventIndex(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t index;

    if (get_event_index(deviceID, &index) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-event-index\n");
    printf("  ID:    %u\n", deviceID);
    printf("  index: %d\n", index);
    printf("\n");

    return 0;
}

int setEventIndex(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t index = EVENT_INDEX;

    if (set_event_index(deviceID, index) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nset-event-index\n");
    printf("  ID:    %u\n", deviceID);
    printf("  index: %d\n", index);
    printf("\n");

    return 0;
}
