#pragma once

#include <stdbool.h>

extern bool getDevices();
extern bool getDevice(uint32_t);
extern bool setAddress(uint32_t, const char *, const char *, const char *);
extern bool getStatus(uint32_t);
