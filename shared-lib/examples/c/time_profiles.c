#include <stdio.h>
#include <stdlib.h>

#include "uhppoted.h"

extern uint32_t DEVICE_ID;
extern uint8_t PROFILE_ID;

int getTimeProfile(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint8_t profileID = PROFILE_ID;
    time_profile profile;

    if (get_time_profile(deviceID, profileID, &profile) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-time-profile\n");
    printf("  ID:                   %u\n", deviceID);
    printf("  profile:              %u\n", profileID);
    printf("  linked profile:       %u\n", profile.linked);
    printf("  enabled from:         %s\n", profile.from);
    printf("          to:           %s\n", profile.to);
    printf("  enabled on Monday:    %d\n", profile.monday);
    printf("             Tuesday:   %d\n", profile.tuesday);
    printf("             Wednesday: %d\n", profile.wednesday);
    printf("             Thursday:  %d\n", profile.thursday);
    printf("             Friday:    %d\n", profile.friday);
    printf("             Saturday:  %d\n", profile.saturday);
    printf("             Sunday:    %d\n", profile.sunday);
    printf("  segment 1:            %s-%s\n", profile.segment1start, profile.segment1end);
    printf("  segment 2:            %s-%s\n", profile.segment2start, profile.segment2end);
    printf("  segment 3:            %s-%s\n", profile.segment3start, profile.segment3end);
    printf("\n");

    return 0;
}

int setTimeProfile(int argc, char **argv) {
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

    if (set_time_profile(deviceID, &profile) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nset-time-profile\n");
    printf("  ID:                   %u\n", deviceID);
    printf("  profile:              %u\n", profile.ID);
    printf("  linked profile:       %u\n", profile.linked);
    printf("  enabled from:         %s\n", profile.from);
    printf("          to:           %s\n", profile.to);
    printf("  enabled on Monday:    %d\n", profile.monday);
    printf("             Tuesday:   %d\n", profile.tuesday);
    printf("             Wednesday: %d\n", profile.wednesday);
    printf("             Thursday:  %d\n", profile.thursday);
    printf("             Friday:    %d\n", profile.friday);
    printf("             Saturday:  %d\n", profile.saturday);
    printf("             Sunday:    %d\n", profile.sunday);
    printf("  segment 1:            %s-%s\n", profile.segment1start, profile.segment1end);
    printf("  segment 2:            %s-%s\n", profile.segment2start, profile.segment2end);
    printf("  segment 3:            %s-%s\n", profile.segment3start, profile.segment3end);
    printf("\n");

    return 0;
}
