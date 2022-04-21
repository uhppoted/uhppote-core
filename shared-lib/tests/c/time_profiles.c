#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tests.h"
#include "uhppoted.h"

bool getTimeProfile() {
    const char *tag = "get-time-profile";
    time_profile profile;

    if (get_time_profile(DEVICE_ID, PROFILE_ID, &profile) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    // clang-format off
    const result resultset[] = {
        { .field = "profile ID",              .type = "uint8",   .value.uint8.expected = 49,            .value.uint8.value  = profile.ID },
        { .field = "linked profile",          .type = "uint8",   .value.uint8.expected = 71,            .value.uint8.value  = profile.linked },
        { .field = "profile 'from' date",     .type = "string",  .value.string.expected = "2022-02-01", .value.string.value  = profile.from },
        { .field = "profile 'to' date",       .type = "string",  .value.string.expected = "2022-06-30", .value.string.value  = profile.to },
        { .field = "profle 'Monday'",         .type = "boolean", .value.boolean.expected = true,        .value.boolean.value = profile.monday },
        { .field = "profle 'Tuesday'",        .type = "boolean", .value.boolean.expected = false,       .value.boolean.value = profile.tuesday },
        { .field = "profle 'Wednesday'",      .type = "boolean", .value.boolean.expected = true,        .value.boolean.value = profile.wednesday },
        { .field = "profle 'Thursday'",       .type = "boolean", .value.boolean.expected = true,        .value.boolean.value = profile.thursday },
        { .field = "profle 'Friday'",         .type = "boolean", .value.boolean.expected = false,       .value.boolean.value = profile.friday },
        { .field = "profle 'Saturday'",       .type = "boolean", .value.boolean.expected = false,       .value.boolean.value = profile.saturday },
        { .field = "profle 'Sunday'",         .type = "boolean", .value.boolean.expected = true,        .value.boolean.value = profile.sunday },
        { .field = "profile seqment 1 start", .type = "string",  .value.string.expected = "08:30",      .value.string.value  = profile.segment1start },
        { .field = "profile segment 1 end",   .type = "string",  .value.string.expected = "11:30",      .value.string.value  = profile.segment1end },
        { .field = "profile seqment 2 start", .type = "string",  .value.string.expected = "",           .value.string.value  = profile.segment2start },
        { .field = "profile segment 2 end",   .type = "string",  .value.string.expected = "",           .value.string.value  = profile.segment2end },
        { .field = "profile seqment 3 start", .type = "string",  .value.string.expected = "14:45",      .value.string.value  = profile.segment3start },
        { .field = "profile segment 3 end",   .type = "string",  .value.string.expected = "",           .value.string.value  = profile.segment3end },
    };
    // clang-format on

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}
