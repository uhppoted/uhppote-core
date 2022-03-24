#pragma once

extern int getDevices();
extern int getDevice(uint32_t);
extern int setAddress(uint32_t, const char *, const char *, const char *);
extern int getStatus(uint32_t);
extern int getTime(uint32_t);
extern int setTime(uint32_t, const char *);
