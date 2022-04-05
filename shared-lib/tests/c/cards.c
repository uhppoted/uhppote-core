#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "device.h"
#include "uhppoted.h"

extern const uint32_t DEVICEID;
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
