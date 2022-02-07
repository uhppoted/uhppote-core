#pragma once

#include <string>
#include <vector>
#include "libuhppote.h"

typedef struct controller {
    unsigned int id;
    std::string  address;
} controller;

typedef struct device {
    unsigned long ID;
    std::string address;
    std::string subnet;
    std::string gateway;
    std::string MAC;
    std::string version;
    std::string date;
} device;


class uhppote {
    public:
        uhppote();
        uhppote(const std::string& bind, const std::string& broadcast, const std::string& listen, int timeout, const std::vector<controller>& controllers, bool debug);
        virtual ~uhppote();

    public:
        std::string errmsg();
        int get_devices(unsigned long **devices, int *N);
        int get_device (unsigned id, struct device *);

    private:
        void set_error(const char *errmsg);

    private:
        UHPPOTE *u;
        std::string err;
};
