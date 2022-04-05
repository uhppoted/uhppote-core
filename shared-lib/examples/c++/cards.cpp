#include <iostream>

#include "device.hpp"

using namespace std;

extern const uint32_t DEVICEID;
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
