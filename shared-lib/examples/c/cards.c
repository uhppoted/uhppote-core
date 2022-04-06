#include <stdio.h>
#include <stdlib.h>

#include "device.h"
#include "uhppoted.h"

extern uint32_t DEVICEID;
extern uint32_t CARDID;

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

int getCard(int argc, char **argv) {
    uint32_t deviceID = DEVICEID;
    uint32_t cardID = CARDID;
    card card;

    if (get_card(deviceID, cardID, &card) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-card\n");
    printf("  ID:           %u\n", deviceID);
    printf("  card number:  %u\n", card.card_number);
    printf("       from:    %s\n", card.from);
    printf("       to:      %s\n", card.to);
    printf("       door[1]: %u\n", card.doors[0]);
    printf("       door[2]: %u\n", card.doors[1]);
    printf("       door[3]: %u\n", card.doors[2]);
    printf("       door[4]: %u\n", card.doors[3]);
    printf("\n");

    return 0;
}
