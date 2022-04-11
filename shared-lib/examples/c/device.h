#pragma once

extern int getDevices(int argc, char **argv);
extern int getDevice(int argc, char **argv);
extern int setAddress(int argc, char **argv);
extern int getStatus(uint32_t);
extern int getTime(uint32_t);
extern int setTime(uint32_t, const char *);
extern int getListener(uint32_t);
extern int setListener(uint32_t, const char *);
extern int getDoorControl(uint32_t,uint8_t);
extern int setDoorControl(uint32_t,uint8_t, uint8_t,uint8_t);
