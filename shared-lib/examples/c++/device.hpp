#pragma once

#include "../include/uhppoted.hpp"

extern int getDevices(uhppoted &);
extern int getDevice(uhppoted &u, int argc, char **argv);
extern int setAddress(uhppoted &, uint32_t, std::string, std::string, std::string);
extern int getStatus(uhppoted &, uint32_t);
extern int getTime(uhppoted &, uint32_t);
extern int setTime(uhppoted &, uint32_t, std::string);
extern int getListener(uhppoted &, uint32_t);
extern int setListener(uhppoted &, uint32_t, std::string);
extern int getDoorControl(uhppoted &, uint32_t, uint8_t);
extern int setDoorControl(uhppoted &, uint32_t, uint8_t, uint8_t,uint8_t);
