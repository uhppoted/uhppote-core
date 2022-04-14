#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_ID;
extern const uint32_t CARD_INDEX;
extern const uint32_t EVENT_INDEX;
extern const uint8_t DOOR;

extern bool result(string test, bool ok);

bool getEventIndex(uhppoted &u) {
    string tag = "set-event-index";
    uint32_t expected = 47;
    bool ok = true;

    try {
        auto index = u.get_event_index(DEVICE_ID);

        if (index != expected) {
            cout << tag << ": incorrect card count - expected:" << expected << ", got:" << index << endl;
            ok = false;
        }

        return result(tag, ok);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}

bool setEventIndex(uhppoted &u) {
    string tag = "set-event-index";

    try {
        u.set_event_index(DEVICE_ID, EVENT_INDEX);

        return result(tag, true);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return false;
}
