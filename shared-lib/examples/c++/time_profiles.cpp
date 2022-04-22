#include <iostream>

#include "../include/uhppoted.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;
extern const uint8_t PROFILE_ID;

int getTimeProfile(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t profileID = PROFILE_ID;

    try {
        time_profile p = u.get_time_profile(deviceID, profileID);

        cout << endl
             << "get-time_profile" << endl;
        cout << "  ID:                   " << deviceID << endl;
        cout << "  profile ID:           " << static_cast<int>(p.ID) << endl;
        cout << "  linked profile:       " << static_cast<int>(p.linked) << endl;
        cout << "  enabled from:         " << p.from << endl;
        cout << "          to:           " << p.to << endl;
        cout << "  enabled on Monday:    " << static_cast<int>(p.monday) << endl;
        cout << "             Tuesday:   " << static_cast<int>(p.tuesday) << endl;
        cout << "             Wednesday: " << static_cast<int>(p.wednesday) << endl;
        cout << "             Thursday:  " << static_cast<int>(p.thursday) << endl;
        cout << "             Friday:    " << static_cast<int>(p.friday) << endl;
        cout << "             Saturday:  " << static_cast<int>(p.saturday) << endl;
        cout << "             Sunday:    " << static_cast<int>(p.sunday) << endl;
        cout << "  segment 1:            " << p.segment1start << "-" << p.segment1end << endl;
        cout << "  segment 2:            " << p.segment2start << "-" << p.segment2end << endl;
        cout << "  segment 3:            " << p.segment3start << "-" << p.segment3end << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int setTimeProfile(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
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

    try {
        u.set_time_profile(deviceID, profile);

        cout << endl
             << "set-time_profile" << endl;
        cout << "  ID:                   " << deviceID << endl;
        cout << "  profile ID:           " << static_cast<int>(profile.ID) << endl;
        cout << "  linked profile:       " << static_cast<int>(profile.linked) << endl;
        cout << "  enabled from:         " << profile.from << endl;
        cout << "          to:           " << profile.to << endl;
        cout << "  enabled on Monday:    " << static_cast<int>(profile.monday) << endl;
        cout << "             Tuesday:   " << static_cast<int>(profile.tuesday) << endl;
        cout << "             Wednesday: " << static_cast<int>(profile.wednesday) << endl;
        cout << "             Thursday:  " << static_cast<int>(profile.thursday) << endl;
        cout << "             Friday:    " << static_cast<int>(profile.friday) << endl;
        cout << "             Saturday:  " << static_cast<int>(profile.saturday) << endl;
        cout << "             Sunday:    " << static_cast<int>(profile.sunday) << endl;
        cout << "  segment 1:            " << profile.segment1start << "-" << profile.segment1end << endl;
        cout << "  segment 2:            " << profile.segment2start << "-" << profile.segment2end << endl;
        cout << "  segment 3:            " << profile.segment3start << "-" << profile.segment3end << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}
