#include <stdio.h>
#include <stdlib.h>

#include "uhppoted.h"

extern uint32_t DEVICE_ID;
extern uint32_t CARD_NUMBER;
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
    uint32_t card_number = CARD_NUMBER;
    card card;

    if (get_card(deviceID, card_number, &card) < 0) {
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

int putCard(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t card_number = CARD_NUMBER;
    char *from = "2022-01-01";
    char *to = "2022-12-31";
    uint8_t doors[4] = {0, 1, 31, 75};

    if (put_card(deviceID, card_number, from, to, doors) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nput-card\n");
    printf("  ID:           %u\n", deviceID);
    printf("  card number:  %u\n", card_number);
    printf("       from:    %s\n", from);
    printf("       to:      %s\n", to);
    printf("       door[1]: %u\n", doors[0]);
    printf("       door[2]: %u\n", doors[1]);
    printf("       door[3]: %u\n", doors[2]);
    printf("       door[4]: %u\n", doors[3]);
    printf("\n");

    return 0;
}

int deleteCard(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t card_number = CARD_NUMBER;

    if (delete_card(deviceID, card_number) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\ndelete-card\n");
    printf("  ID:           %u\n", deviceID);
    printf("  card number:  %u\n", card_number);
    printf("\n");

    return 0;
}

int deleteCards(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    if (delete_cards(deviceID) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\ndelete-cards\n");
    printf("  ID: %u\n", deviceID);
    printf("\n");

    return 0;
}
