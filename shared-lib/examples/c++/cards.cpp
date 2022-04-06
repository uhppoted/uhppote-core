#include <iostream>

#include "device.hpp"

using namespace std;

extern const uint32_t DEVICEID;
extern const uint32_t CARDID;
extern const uint8_t DOOR;

int getCards(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICEID;

    try {
        int N = u.get_cards(deviceID);

        cout << endl
             << "get-cards" << endl;
        cout << "  ID:    " << deviceID << endl;
        cout << "  cards: " << N << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int getCard(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICEID;
    uint32_t cardID = CARDID;

    try {
        card c = u.get_card(deviceID, cardID);

        cout << endl
             << "get-card" << endl;
        cout << "  ID:           " << deviceID << endl;
        cout << "  card number:  " << c.card_number << endl;
        cout << "       from:    " << c.from << endl;
        cout << "       to:      " << c.to << endl;
        cout << "       door[1]: " << static_cast<int>(c.doors[0]) << endl;
        cout << "       door[2]: " << static_cast<int>(c.doors[1]) << endl;
        cout << "       door[3]: " << static_cast<int>(c.doors[2]) << endl;
        cout << "       door[4]: " << static_cast<int>(c.doors[3]) << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}
