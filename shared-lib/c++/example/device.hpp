#pragma once

#include "../include/uhppoted.hpp"

extern int getDevices(uhppoted &);
extern int getDevice(uhppoted &, uint32_t);
extern int setAddress(uhppoted &, uint32_t, std::string, std::string, std::string);
