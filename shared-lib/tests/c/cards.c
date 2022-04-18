#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uhppoted.h"

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_NUMBER;
extern const uint32_t CARD_INDEX;
extern const uint8_t DOOR;

extern bool passed(const char *);
extern bool failed(const char *);

bool getCards() {
    const char *tag = "get-cards";
    int expected = 39;
    int N;

    if (get_cards(DEVICE_ID, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (N != expected) {
        printf("%s: incorrect card count - expected:%u, got:%u\n", tag, expected, N);
        ok = false;
    }

    if (!ok) {
        return failed(tag);
    }

    return passed(tag);
}

bool getCard() {
    const char *tag = "get-card";
    card card;

    if (get_card(DEVICE_ID, CARD_NUMBER, &card) < 0) {
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

    if (!ok) {
        return failed(tag);
    }

    return passed(tag);
}

bool getCardByIndex() {
    const char *tag = "get-card-by-index";
    card card;

    if (get_card_by_index(DEVICE_ID, CARD_INDEX, &card) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    bool ok = true;

    if (card.card_number != 8165538) {
        printf("get-card-by-index: incorrect card number - expected:%u, got:%u\n", 8198765, card.card_number);
        ok = false;
    }

    if (strcmp(card.from, "2022-01-01") != 0) {
        printf("get-card-by-index: incorrect card 'from' date - expected:%s, got:%s\n", "2022-01-01", card.from);
        ok = false;
    }

    if (strcmp(card.to, "2022-12-31") != 0) {
        printf("get-card-by-index: incorrect card 'to' date - expected:%s, got:%s\n", "2022-12-31", card.to);
        ok = false;
    }

    if (card.doors[0] != 0) {
        printf("get-card-by-index: incorrect card doors[1] - expected:%u, got:%u\n", 0, card.doors[0]);
        ok = false;
    }

    if (card.doors[1] != 1) {
        printf("get-card-by-index: incorrect card doors[2] - expected:%u, got:%u\n", 1, card.doors[1]);
        ok = false;
    }

    if (card.doors[2] != 31) {
        printf("get-card-by-index: incorrect card doors[3] - expected:%u, got:%u\n", 31, card.doors[2]);
        ok = false;
    }

    if (card.doors[3] != 75) {
        printf("get-card-by-index: incorrect card doors[4] - expected:%u, got:%u\n", 75, card.doors[3]);
        ok = false;
    }

    if (!ok) {
        return failed(tag);
    }

    return passed(tag);
}

bool putCard() {
    const char *tag = "put-card";
    uint8_t doors[4] = {0, 1, 31, 75};

    if (put_card(DEVICE_ID, CARD_NUMBER, "2022-01-01", "2022-12-31", (uint8_t *)doors) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    return passed(tag);
}

bool deleteCard() {
    const char *tag = "delete-card";

    if (delete_card(DEVICE_ID, CARD_NUMBER) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    return passed(tag);
}

bool deleteCards() {
    const char *tag = "delete-cards";

    if (delete_cards(DEVICE_ID) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    return passed(tag);
}
