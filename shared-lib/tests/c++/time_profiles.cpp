#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool getTimeProfile(uhppoted &u) {
    auto profile = u.get_time_profile(DEVICE_ID, PROFILE_ID);

    vector<result> rs = {
        result("profile ID", uint8_t(49), profile.ID),
        result("linked profile", uint8_t(71), profile.linked),
        result("profile 'from' date", string("2022-02-01"), profile.from),
        result("profile 'to' date", string("2022-06-30"), profile.to),
        result("profile Monday", true, profile.monday),
        result("profile Tuesday", false, profile.tuesday),
        result("profile Wednesday", true, profile.wednesday),
        result("profile Thursday", true, profile.thursday),
        result("profile Friday", false, profile.friday),
        result("profile Saturday", false, profile.saturday),
        result("profile Sunday", true, profile.sunday),
        result("profile segment 1 start", string("08:30"), profile.segment1start),
        result("profile segment 1 end", string("11:30"), profile.segment1end),
        result("profile segment 2 start", string("00:00"), profile.segment2start),
        result("profile segment 2 end", string("00:00"), profile.segment2end),
        result("profile segment 3 start", string("00:00"), profile.segment3start),
        result("profile segment 3 end", string("18:00"), profile.segment3end),
    };

    return evaluate("get-time-profile", rs);
}

bool setTimeProfile(uhppoted &u) {
    time_profile profile = {
        .ID = PROFILE_ID,
        .linked = 71,
        .from = "2022-02-01",
        .to = "2022-06-30",
        .monday = true,
        .tuesday = false,
        .wednesday = true,
        .thursday = true,
        .friday = false,
        .saturday = false,
        .sunday = true,
        .segment1start = "08:30",
        .segment1end = "11:30",
        .segment2start = "",
        .segment2end = "",
        .segment3start = "",
        .segment3end = "18:00",
    };

    u.set_time_profile(DEVICE_ID, profile);

    vector<result> rs = {};

    return evaluate("set-time-profile", rs);
}
