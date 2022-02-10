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
        std::vector<uint32_t> get_devices();
        device get_device (uint32_t id);

    private:
        UHPPOTE *u;
};
