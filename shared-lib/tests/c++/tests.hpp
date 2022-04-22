#pragma once

#include <any>
#include <string>
#include <tuple>
#include <vector>

#include "../include/uhppoted.hpp"

typedef std::tuple<std::string, std::any, std::any> result;

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_ID;
extern const uint32_t CARD_INDEX;
extern const uint32_t EVENT_INDEX;
extern const uint8_t DOOR;
extern const uint8_t PROFILE_ID;

extern bool getDevices(uhppoted &);
extern bool getDevice(uhppoted &);
extern bool setAddress(uhppoted &);
extern bool getStatus(uhppoted &);
extern bool getTime(uhppoted &);
extern bool setTime(uhppoted &);
extern bool getListener(uhppoted &);
extern bool setListener(uhppoted &);
extern bool getDoorControl(uhppoted &);
extern bool setDoorControl(uhppoted &);
extern bool openDoor(uhppoted &);

extern bool getCards(uhppoted &);
extern bool getCard(uhppoted &);
extern bool getCardByIndex(uhppoted &);
extern bool putCard(uhppoted &);
extern bool deleteCard(uhppoted &);
extern bool deleteCards(uhppoted &);

extern bool getEventIndex(uhppoted &);
extern bool setEventIndex(uhppoted &);
extern bool getEvent(uhppoted &);
extern bool recordSpecialEvents(uhppoted &);

extern bool getTimeProfile(uhppoted &);
extern bool setTimeProfile(uhppoted &);

extern bool evaluate(const std::string &, const std::vector<result> &);
extern bool passed(const std::string &);
extern bool failed(const std::string &);
