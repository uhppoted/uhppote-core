#include <iomanip>
#include <iostream>

#include "cards.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_ID;
extern const uint32_t CARD_INDEX;
extern const uint8_t DOOR;

extern bool result(string test, bool ok);

bool getCards(uhppoted &u) {
    try {
        auto N = u.get_cards(DEVICE_ID);
        bool ok = true;

        if (N != 39) {
            cout << "get-cards: incorrect card count - expected:" << 39 << ", got:" << N << endl;
            ok = false;
        }

        return result("get-cards", ok);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool getCard(uhppoted &u) {
    try {
        auto card = u.get_card(DEVICE_ID, CARD_ID);
        bool ok = true;

        if (card.card_number != 8165538) {
            cout << "get-card: incorrect card number - expected:" << 8165538 << ", got:" << card.card_number << endl;
            ok = false;
        }

        if (card.from != "2022-01-01") {
            cout << "get-card: incorrect card 'from' date - expected:"
                 << "2022-01-01"
                 << ", got:" << card.from << endl;
            ok = false;
        }

        if (card.to != "2022-12-31") {
            cout << "get-card: incorrect card 'to' date - expected:"
                 << "2022-12-31"
                 << ", got:" << card.to << endl;
            ok = false;
        }

        if (card.doors[0] != 0) {
            cout << "get-card: incorrect doors[1] - expected:" << 0 << ", got:" << static_cast<int>(card.doors[0]) << endl;
            ok = false;
        }

        if (card.doors[1] != 1) {
            cout << "get-card: incorrect doors[2] - expected:" << 1 << ", got:" << static_cast<int>(card.doors[1]) << endl;
            ok = false;
        }

        if (card.doors[2] != 31) {
            cout << "get-card: incorrect doors[3] - expected:" << 31 << ", got:" << static_cast<int>(card.doors[2]) << endl;
            ok = false;
        }

        if (card.doors[3] != 75) {
            cout << "get-card: incorrect doors[4] - expected:" << 75 << ", got:" << static_cast<int>(card.doors[3]) << endl;
            ok = false;
        }

        return result("get-card", ok);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool getCardByIndex(uhppoted &u) {
    try {
        auto card = u.get_card_by_index(DEVICE_ID, CARD_INDEX);
        bool ok = true;

        if (card.card_number != 8165538) {
            cout << "get-card-by-index: incorrect card number - expected:" << 8165538 << ", got:" << card.card_number << endl;
            ok = false;
        }

        if (card.from != "2022-01-01") {
            cout << "get-card-by-index: incorrect card 'from' date - expected:"
                 << "2022-01-01"
                 << ", got:" << card.from << endl;
            ok = false;
        }

        if (card.to != "2022-12-31") {
            cout << "get-card-by-index: incorrect card 'to' date - expected:"
                 << "2022-12-31"
                 << ", got:" << card.to << endl;
            ok = false;
        }

        if (card.doors[0] != 0) {
            cout << "get-card-by-index: incorrect doors[1] - expected:" << 0 << ", got:" << static_cast<int>(card.doors[0]) << endl;
            ok = false;
        }

        if (card.doors[1] != 1) {
            cout << "get-card-by-index: incorrect doors[2] - expected:" << 1 << ", got:" << static_cast<int>(card.doors[1]) << endl;
            ok = false;
        }

        if (card.doors[2] != 31) {
            cout << "get-card-by-index: incorrect doors[3] - expected:" << 31 << ", got:" << static_cast<int>(card.doors[2]) << endl;
            ok = false;
        }

        if (card.doors[3] != 75) {
            cout << "get-card-by-index: incorrect doors[4] - expected:" << 75 << ", got:" << static_cast<int>(card.doors[3]) << endl;
            ok = false;
        }

        return result("get-card-by-index", ok);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}
