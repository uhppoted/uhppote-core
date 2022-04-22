#pragma once

#include "libuhppoted.h"
#include <string>
#include <vector>

#define NORMALLY_OPEN 1
#define NORMALLY_CLOSED 2
#define CONTROLLED 3

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
    std::string timestamp;
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
    std::string sysdatetime;
    uint8_t doors[4];
    uint8_t buttons[4];
    uint8_t relays;
    uint8_t inputs;
    uint8_t syserror;
    uint8_t info;
    uint32_t seqno;
    event event;
} status;

typedef struct door_control {
    uint8_t mode;
    uint8_t delay;
} door_control;

typedef struct card {
    uint32_t card_number;
    std::string from;
    std::string to;
    uint8_t doors[4];
} card;

typedef struct time_profile {
    uint8_t ID;
    uint8_t linked;
    std::string from;
    std::string to;
    bool monday;
    bool tuesday;
    bool wednesday;
    bool thursday;
    bool friday;
    bool saturday;
    bool sunday;
    std::string segment1start;
    std::string segment1end;
    std::string segment2start;
    std::string segment2end;
    std::string segment3start;
    std::string segment3end;
} time_profile;

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
    door_control get_door_control(uint32_t id, uint8_t door);
    void set_door_control(uint32_t id, uint8_t door, uint8_t mode, uint8_t delay);
    void open_door(uint32_t id, uint8_t door);

    int get_cards(uint32_t id);
    card get_card(uint32_t id, uint32_t card_number);
    card get_card_by_index(uint32_t id, uint32_t index);
    void put_card(uint32_t id, uint32_t card_number, std::string from, std::string to, uint8_t doors[4]);
    void delete_card(uint32_t id, uint32_t card_number);
    void delete_cards(uint32_t id);
    uint32_t get_event_index(uint32_t id);
    void set_event_index(uint32_t id, uint32_t index);
    event get_event(uint32_t id, uint32_t index);
    void record_special_events(uint32_t id, bool enabled);

    time_profile get_time_profile(uint32_t id, uint8_t profile_id);
    void set_time_profile(uint32_t id, const time_profile &profile);

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