#pragma once

#include "libuhppoted.h"
#include <string>
#include <vector>

typedef struct controller {
    uint32_t id;
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

typedef struct event {
    char timestamp[20];
    uint32_t index;
    uint8_t eventType;
    bool granted;
    uint8_t door;
    uint8_t direction;
    uint32_t card;
    uint8_t reason;
} event;

typedef struct status {
    uint32_t ID;
    char sysdatetime[20];
    uint8_t doors[4];
    uint8_t buttons[4];
    uint8_t relays;
    uint8_t inputs;
    uint8_t syserror;
    uint8_t info;
    uint32_t seqno;
    event event;
} status;

class uhppoted {
  public:
    uhppoted();
    uhppoted(const std::string &bind, const std::string &broadcast,
             const std::string &listen, int timeout,
             const std::vector<controller> &controllers, bool debug);
    virtual ~uhppoted();

  public:
    std::vector<uint32_t> get_devices();
    device get_device(uint32_t id);
    void set_address(uint32_t id, std::string &address, std::string &subnet,
                     std::string &gateway);
    status get_status(uint32_t id);
    std::string get_time(uint32_t id);
    void set_time(uint32_t id, std::string &);
    std::string get_listener(uint32_t id);
    void set_listener(uint32_t id, std::string &);

  private:
    UHPPOTE *u;
};

// Ref. https://www.boost.org/community/error_handling.html
class uhppoted_exception : public virtual std::exception {
  public:
    uhppoted_exception(char *);
    virtual ~uhppoted_exception();

    virtual const char *what() const noexcept;

  private:
    std::shared_ptr<char *> message;
};