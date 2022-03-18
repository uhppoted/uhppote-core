#pragma once

#include "../include/uhppoted.hpp"

extern int getDevices(uhppoted &);
extern int getDevice(uhppoted &, uint32_t);
extern int setAddress(uhppoted &, uint32_t, std::string, std::string, std::string);
extern int getStatus(uhppoted &, uint32_t);
extern int getTime(uhppoted &, uint32_t);