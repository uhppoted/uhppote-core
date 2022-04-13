#pragma once

#include "../include/uhppoted.hpp"

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

extern bool getCards(uhppoted &);
extern bool getCard(uhppoted &);
extern bool getCardByIndex(uhppoted &);
extern bool putCard(uhppoted &);
extern bool deleteCard(uhppoted &);
extern bool deleteCards(uhppoted &);

extern bool getEventIndex(uhppoted &);
