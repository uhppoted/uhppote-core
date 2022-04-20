#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool getCards(uhppoted &u) {
    string tag = "get-cards";
    bool ok = true;

    try {
        auto N = u.get_cards(DEVICE_ID);

        if (N != 39) {
            cout << tag << ": incorrect card count - expected:" << 39 << ", got:" << N << endl;
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);

    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool getCard(uhppoted &u) {
    string tag = "get-card";
    bool ok = true;

    try {
        auto card = u.get_card(DEVICE_ID, CARD_ID);

        if (card.card_number != 8165538) {
            cout << tag << ": incorrect card number - expected:" << 8165538 << ", got:" << card.card_number << endl;
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

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);

    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool getCardByIndex(uhppoted &u) {
    string tag = "get-card-by-index";
    bool ok = true;

    try {
        auto card = u.get_card_by_index(DEVICE_ID, CARD_INDEX);

        if (card.card_number != 8165538) {
            cout << tag << ": incorrect card number - expected:" << 8165538 << ", got:" << card.card_number << endl;
            ok = false;
        }

        if (card.from != "2022-01-01") {
            cout << tag << ": incorrect card 'from' date - expected:"
                 << "2022-01-01"
                 << ", got:" << card.from << endl;
            ok = false;
        }

        if (card.to != "2022-12-31") {
            cout << tag << ": incorrect card 'to' date - expected:"
                 << "2022-12-31"
                 << ", got:" << card.to << endl;
            ok = false;
        }

        if (card.doors[0] != 0) {
            cout << tag << ": incorrect doors[1] - expected:" << 0 << ", got:" << static_cast<int>(card.doors[0]) << endl;
            ok = false;
        }

        if (card.doors[1] != 1) {
            cout << tag << ": incorrect doors[2] - expected:" << 1 << ", got:" << static_cast<int>(card.doors[1]) << endl;
            ok = false;
        }

        if (card.doors[2] != 31) {
            cout << tag << ": incorrect doors[3] - expected:" << 31 << ", got:" << static_cast<int>(card.doors[2]) << endl;
            ok = false;
        }

        if (card.doors[3] != 75) {
            cout << tag << ": incorrect doors[4] - expected:" << 75 << ", got:" << static_cast<int>(card.doors[3]) << endl;
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);

    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool putCard(uhppoted &u) {
    string tag = "put-card";

    try {
        uint8_t doors[4] = {0, 1, 31, 75};

        u.put_card(DEVICE_ID, CARD_ID, "2022-01-01", "2022-12-31", doors);

        return passed(tag);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool deleteCard(uhppoted &u) {
    string tag = "delete-card";

    try {
        u.delete_card(DEVICE_ID, CARD_ID);

        return passed(tag);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool deleteCards(uhppoted &u) {
    string tag = "delete-cards";

    try {
        u.delete_cards(DEVICE_ID);

        return passed(tag);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}
