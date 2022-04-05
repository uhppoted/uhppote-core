#include <stdio.h>
#include <stdlib.h>

#include "device.h"
#include "uhppoted.h"

extern uint32_t DEVICEID;

int getCards(int argc, char **argv) {
    uint32_t deviceID = DEVICEID;
    int N;

    if (get_cards(deviceID, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-cards\n");
    printf("  ID:    %u\n", deviceID);
    printf("  cards: %d\n", N);
    printf("\n");

    return 0;
}
