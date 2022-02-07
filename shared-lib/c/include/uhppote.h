#pragma once

#include <stdbool.h> 

typedef struct controller {
    unsigned    id;
    const char *address;
} controller;

typedef struct device {
    unsigned long ID;
    char address[16];
    char subnet[16];
    char gateway[16];
    char MAC[18];
    char version[6];
    char date[11];
} device;


char *errmsg();
void setup(const char *bind, const char *broadcast, const char *listen, int timeout, int debug, ...);
void teardown();

int   get_devices(unsigned long **devices, int *N);
int   get_device (unsigned id, struct device *);

