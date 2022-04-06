#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "device.h"
#include "uhppoted.h"

extern const uint32_t DEVICEID;
extern const uint32_t CARDID;
extern const uint8_t DOOR;
extern bool result(char *test, bool ok);

bool getCards() {
    int N;

    if (get_cards(DEVICEID, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (N != 39) {
        printf("get-cards: incorrect card count - expected:%u, got:%u\n", 39, N);
        ok = false;
    }

    return result("get-cards", ok);
}

bool getCard() {
    card card;

    if (get_card(DEVICEID, CARDID, &card) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (card.card_number != 8165538) {
        printf("get-card: incorrect card number - expected:%u, got:%u\n", 8198765, card.card_number);
        ok = false;
    }

    if (strcmp(card.from, "2022-01-01") != 0) {
        printf("get-card: incorrect card 'from' date - expected:%s, got:%s\n", "2022-01-01", card.from);
        ok = false;
    }

    if (strcmp(card.to, "2022-12-31") != 0) {
        printf("get-card: incorrect card 'to' date - expected:%s, got:%s\n", "2022-12-31", card.to);
        ok = false;
    }

    if (card.doors[0] != 0) {
        printf("get-card: incorrect card doors[1] - expected:%u, got:%u\n", 0, card.doors[0]);
        ok = false;
    }

    if (card.doors[1] != 1) {
        printf("get-card: incorrect card doors[2] - expected:%u, got:%u\n", 1, card.doors[1]);
        ok = false;
    }

    if (card.doors[2] != 31) {
        printf("get-card: incorrect card doors[3] - expected:%u, got:%u\n", 31, card.doors[2]);
        ok = false;
    }

    if (card.doors[3] != 75) {
        printf("get-card: incorrect card doors[4] - expected:%u, got:%u\n", 75, card.doors[3]);
        ok = false;
    }

    return result("get-card", ok);
}
