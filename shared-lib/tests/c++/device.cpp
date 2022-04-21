#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool getDevices(uhppoted &u) {
    auto devices = u.get_devices();

    vector<result> rs = {
        result("device count", uint32_t(3), uint32_t(devices.size())),
        result("device[0] ID", uint32_t(201020304), devices[0]),
        result("device[1] ID", uint32_t(303986753), devices[1]),
        result("device[2] ID", uint32_t(405419896), devices[2]),
    };

    return evaluate("get-devices", rs);
}

bool getDevice(uhppoted &u) {
    auto d = u.get_device(DEVICE_ID);

    vector<result> rs = {
        result("device ID", uint32_t(405419896), d.ID),
        result("IP address", string("192.168.1.101"), d.address),
        result("subnet mask", string("255.255.255.0"), d.subnet),
        result("gateway address", string("192.168.1.1"), d.gateway),
        result("MAC address", string("00:12:23:34:45:56"), d.MAC),
        result("version", string("v8.92"), d.version),
        result("date", string("2018-11-05"), d.date),
    };

    return evaluate("get-device", rs);
}

bool setAddress(uhppoted &u) {
    string address = "192.168.1.125";
    string subnet = "255.255.254.0";
    string gateway = "192.168.1.0";

    u.set_address(DEVICE_ID, address, subnet, gateway);

    vector<result> rs = {};

    return evaluate("set-address", rs);
}

bool getStatus(uhppoted &u) {
    auto s = u.get_status(DEVICE_ID);

    vector<result> rs = {
        result("device ID", uint32_t(405419896), s.ID),
        result("system date/time", string("2022-03-19 15:48:32"), s.sysdatetime),
        result("doors[1]", true, s.doors[0] != 0),
        result("doors[2]", false, s.doors[1] != 0),
        result("doors[3]", false, s.doors[2] != 0),
        result("doors[4]", true, s.doors[3] != 0),
        result("buttons[1]", true, s.buttons[0] != 0),
        result("buttons[2]", false, s.buttons[1] != 0),
        result("buttons[3]", true, s.buttons[2] != 0),
        result("buttons[4]", false, s.buttons[3] != 0),
        result("relays", uint8_t(0x12), s.relays),
        result("inputs", uint8_t(0x34), s.inputs),
        result("system error", uint8_t(0x56), s.syserror),
        result("special info", uint8_t(253), s.info),
        result("sequence number", uint32_t(9876), s.seqno),
        result("event timestamp", string("2022-01-02 12:34:56"), s.event.timestamp),
        result("event index", uint32_t(135), s.event.index),
        result("event type", uint8_t(6), s.event.eventType),
        result("event granted", true, s.event.granted),
        result("event door", uint8_t(3), s.event.door),
        result("event direction", uint8_t(1), s.event.direction),
        result("event card", uint32_t(8100023), s.event.card),
        result("event reason", uint8_t(21), s.event.reason),
    };

    return evaluate("get-status", rs);
}

bool getTime(uhppoted &u) {
    string datetime = u.get_time(DEVICE_ID);

    vector<result> rs = {
        result("date/time", string("2022-01-02 12:34:56"), datetime),
    };

    return evaluate("get-time", rs);
}

bool setTime(uhppoted &u) {
    string datetime = "2022-03-23 12:24:17";

    u.set_time(DEVICE_ID, datetime);

    vector<result> rs = {};

    return evaluate("set-time", rs);
}

bool getListener(uhppoted &u) {
    string listener = u.get_listener(DEVICE_ID);

    vector<result> rs = {
        result("listener address", string("192.168.1.100:60001"), listener),
    };

    return evaluate("get-listener", rs);
}

bool setListener(uhppoted &u) {
    string listener = "192.168.1.100:60001";

    u.set_listener(DEVICE_ID, listener);

    vector<result> rs = {};

    return evaluate("set-listener", rs);
}

bool getDoorControl(uhppoted &u) {
    auto d = u.get_door_control(DEVICE_ID, DOOR);

    vector<result> rs = {
        result("door control mode", uint8_t(CONTROLLED), d.mode),
        result("door delay", uint8_t(7), d.delay),
    };

    return evaluate("get-door-control", rs);
}

bool setDoorControl(uhppoted &u) {
    u.set_door_control(DEVICE_ID, DOOR, NORMALLY_CLOSED, 6);

    vector<result> rs = {};

    return evaluate("set-door-control", rs);
}

bool openDoor(uhppoted &u) {
    u.open_door(DEVICE_ID, DOOR);

    vector<result> rs = {};

    return evaluate("open-door", rs);
}
