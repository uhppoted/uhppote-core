#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tests.h"
#include "uhppoted.h"

bool getCards() {
    const char *tag = "get-cards";
    int N;

    if (get_cards(DEVICE_ID, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {
        {
            .field = "card count",
            .type = "uint32",
            .value.uint32.expected = 39,
            .value.uint32.value = N,
        },
    };

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool getCard() {
    const char *tag = "get-card";
    card card;

    if (get_card(DEVICE_ID, CARD_NUMBER, &card) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    // clang-format off
    const result resultset[] = {
        { .field = "card number",      .type = "uint32", .value.uint32.expected = 8165538,      .value.uint32.value = card.card_number },
        { .field = "card 'from' date", .type = "string", .value.string.expected = "2022-01-01", .value.string.value = card.from },
        { .field = "card 'to' date",   .type = "string", .value.string.expected = "2022-12-31", .value.string.value = card.to },
        { .field = "card doors[1]",    .type = "uint8",  .value.uint8.expected = 0,             .value.uint8.value = card.doors[0] },
        { .field = "card doors[2]",    .type = "uint8",  .value.uint8.expected = 1,             .value.uint8.value = card.doors[1] },
        { .field = "card doors[3]",    .type = "uint8",  .value.uint8.expected = 31,            .value.uint8.value = card.doors[2] },
        { .field = "card doors[4]",    .type = "uint8",  .value.uint8.expected = 75,            .value.uint8.value = card.doors[3] },
    };
    // clang-format on

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool getCardByIndex() {
    const char *tag = "get-card-by-index";
    card card;

    if (get_card_by_index(DEVICE_ID, CARD_INDEX, &card) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    // clang-format off
    const result resultset[] = {
        { .field = "card number",      .type = "uint32", .value.uint32.expected = 8165538,      .value.uint32.value = card.card_number },
        { .field = "card 'from' date", .type = "string", .value.string.expected = "2022-01-01", .value.string.value = card.from },
        { .field = "card 'to' date",   .type = "string", .value.string.expected = "2022-12-31", .value.string.value = card.to },
        { .field = "card doors[1]",    .type = "uint8",  .value.uint8.expected = 0,             .value.uint8.value = card.doors[0] },
        { .field = "card doors[2]",    .type = "uint8",  .value.uint8.expected = 1,             .value.uint8.value = card.doors[1] },
        { .field = "card doors[3]",    .type = "uint8",  .value.uint8.expected = 31,            .value.uint8.value = card.doors[2] },
        { .field = "card doors[4]",    .type = "uint8",  .value.uint8.expected = 75,            .value.uint8.value = card.doors[3] },
    };
    // clang-format on

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool putCard() {
    const char *tag = "put-card";
    uint8_t doors[4] = {0, 1, 31, 75};

    if (put_card(DEVICE_ID, CARD_NUMBER, "2022-01-01", "2022-12-31", (uint8_t *)doors) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool deleteCard() {
    const char *tag = "delete-card";

    if (delete_card(DEVICE_ID, CARD_NUMBER) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool deleteCards() {
    const char *tag = "delete-cards";

    if (delete_cards(DEVICE_ID) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}
