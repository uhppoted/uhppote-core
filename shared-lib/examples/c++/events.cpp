#include <iostream>

#include "../include/uhppoted.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_NUMBER;
extern const uint32_t CARD_INDEX;
extern const uint8_t DOOR;

int getEventIndex(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    try {
        int index = u.get_event_index(deviceID);

        cout << endl
             << "get-event-index" << endl;
        cout << "  ID:    " << deviceID << endl;
        cout << "  index: " << index << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}
