#pragma once

#include <stdbool.h>

extern bool getDevices();
extern bool getDevice();
extern bool setAddress();
extern bool getStatus();
extern bool getTime();
extern bool setTime();
extern bool getListener();
extern bool setListener();
extern bool getDoorControl();
extern bool setDoorControl();

extern bool getCards();
extern bool getCard();
extern bool getCardByIndex();
extern bool putCard();
extern bool deleteCard();
extern bool deleteCards();

extern bool getEventIndex();
extern bool setEventIndex();
extern bool getEvent();
