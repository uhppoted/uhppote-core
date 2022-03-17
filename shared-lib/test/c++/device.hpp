#pragma once

#include "../include/uhppoted.hpp"

extern bool getDevices(uhppoted &);
extern bool getDevice(uhppoted &, uint32_t);
extern bool setAddress(uhppoted &, uint32_t, std::string, std::string, std::string);
extern bool getStatus(uhppoted &, uint32_t);
