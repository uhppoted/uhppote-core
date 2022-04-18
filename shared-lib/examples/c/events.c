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

int getEvent(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t index = EVENT_INDEX;
    event event;

    if (get_event(deviceID, index, &event) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-event\n");
    printf("  ID:                %u\n", deviceID);
    printf("  event index:       %u\n", event.index);
    printf("        timestamp:   %s\n", event.timestamp);
    printf("        type:        %u\n", event.eventType);
    printf("        granted:     %d\n", event.granted);
    printf("        direction:   %u\n", event.direction);
    printf("        card number: %u\n", event.card);
    printf("        reason:      %u\n", event.reason);
    printf("\n");

    return 0;
}

int recordSpecialEvents(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    bool enabled = true;

    if (record_special_events(deviceID, enabled) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nrecord-special-events\n");
    printf("  ID:      %u\n", deviceID);
    printf("  enabled: %d\n", enabled);
    printf("\n");

    return 0;
}
