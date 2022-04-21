#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool getEventIndex(uhppoted &u) {
    auto index = u.get_event_index(DEVICE_ID);

    vector<result> rs = {
        result("event index", uint32_t(47), index),
    };

    return evaluate("get-event-index", rs);
}

bool setEventIndex(uhppoted &u) {
    u.set_event_index(DEVICE_ID, EVENT_INDEX);

    vector<result> rs = {};

    return evaluate("set-event-index", rs);
}

bool getEvent(uhppoted &u) {
    auto event = u.get_event(DEVICE_ID, EVENT_INDEX);

    vector<result> rs = {
        result("event index", uint32_t(51), event.index),
        result("event timestamp", string("2022-04-15 12:29:15"), event.timestamp),
        result("event granted", uint8_t(6), event.eventType),
        result("event granted", true, event.granted),
        result("event door", uint8_t(3), event.door),
        result("event direction", uint8_t(1), event.direction),
        result("event card", uint32_t(8165538), event.card),
        result("event reason", uint8_t(21), event.reason),
    };

    return evaluate("get-event-index", rs);
}

bool recordSpecialEvents(uhppoted &u) {
    u.record_special_events(DEVICE_ID, true);

    vector<result> rs = {};

    return evaluate("record-special-events", rs);
}
