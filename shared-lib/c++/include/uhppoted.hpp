#pragma once

#include <string>
#include <vector>
#include "libuhppoted.h"

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

class uhppoted {
    public:
                 uhppoted();
                 uhppoted(const std::string& bind, const std::string& broadcast, const std::string& listen, int timeout, const std::vector<controller>& controllers, bool debug);
        virtual ~uhppoted();

    public:
        std::vector<uint32_t> get_devices();
        device get_device (uint32_t id);

    private:
        UHPPOTE *u;
};

class uhppoted_exception : public std::exception {
    public:
                 uhppoted_exception(char *); 
        virtual ~uhppoted_exception();

        virtual const char* what() const noexcept;

    private:
        std::string message;
};