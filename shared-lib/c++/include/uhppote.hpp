#pragma once

#include <string>
#include <vector>
#include "libuhppote.h"

typedef struct controller {
    uint32_t    id;
    std::string address;
} controller;

typedef struct device {
    uint32_t ID;
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
        int get_devices(std::vector<uint32_t>& devices);
        int get_device (uint32_t id, struct device&);

    private:
        void set_error(const char *errmsg);

    private:
        UHPPOTE *u;
        std::string err;
};
