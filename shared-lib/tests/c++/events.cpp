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
            cout << tag << ": incorrect event index - expected:" << expected << ", got:" << index << endl;
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

bool getEvent(uhppoted &u) {
    string tag = "set-event";
    bool ok = true;

    try {
        auto event = u.get_event(DEVICE_ID, EVENT_INDEX);

        if (event.index != 51) {
            cout << tag << ": incorrect event index - expected:" << 51 << ", got:" << event.index << endl;
            ok = false;
        }

        if (event.timestamp != "2022-04-15 12:29:15") {
            cout << tag << ": incorrect event timestamp - expected:"
                 << "2022-01-01"
                 << ", got:" << event.timestamp << endl;
            ok = false;
        }

        if (event.eventType != 6) {
            cout << tag << ": incorrect event type - expected:" << 6 << ", got:" << event.eventType << endl;
            ok = false;
        }

        if (!event.granted) {
            cout << tag << ": incorrect event granted - expected:" << 1 << ", got:" << event.granted << endl;
            ok = false;
        }

        if (event.door != 3) {
            cout << tag << ": incorrect event door - expected:" << 3 << ", got:" << event.door << endl;
            ok = false;
        }

        if (event.direction != 1) {
            cout << tag << ": incorrect event direction - expected:" << 1 << ", got:" << event.direction << endl;
            ok = false;
        }

        if (event.card != 8165538) {
            cout << tag << ": incorrect event card number - expected:" << 8165538 << ", got:" << event.card << endl;
            ok = false;
        }

        if (event.reason != 21) {
            cout << tag << ": incorrect event reason - expected:" << 21 << ", got:" << event.reason << endl;
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
