#include <stdio.h>
#include <stdlib.h>

#include "device.h"
#include "uhppoted.h"

extern uint32_t DEVICE_ID;
extern uint32_t CARD_ID;
extern uint32_t CARD_INDEX;

int getCards(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
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
    uint32_t deviceID = DEVICE_ID;
    uint32_t cardID = CARD_ID;
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

int getCardByIndex(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t index = CARD_INDEX;
    card card;

    if (get_card_by_index(deviceID, index, &card) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-card-by-index\n");
    printf("  ID:           %u\n", deviceID);
    printf("  index:        %u\n", index);
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
