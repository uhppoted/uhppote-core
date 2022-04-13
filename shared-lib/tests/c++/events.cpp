#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_ID;
extern const uint32_t CARD_INDEX;
extern const uint8_t DOOR;

extern bool result(string test, bool ok);

bool getEventIndex(uhppoted &u) {
    try {
        auto index = u.get_event_index(DEVICE_ID);
        uint32_t expected = 47;
        bool ok = true;

        if (index != expected) {
            cout << "get-event-index: incorrect card count - expected:" << expected << ", got:" << index << endl;
            ok = false;
        }

        return result("get-event-index", ok);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}
