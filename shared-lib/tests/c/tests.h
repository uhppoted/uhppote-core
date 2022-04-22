#pragma once

#include <stdbool.h>

typedef struct uint8 {
            uint8_t expected;
            uint8_t value;
        } uint8;

typedef struct uint32 {
            uint32_t expected;
            uint32_t value;
        } uint32;

typedef struct boolean {
            bool expected;
            bool value;
        } boolean;

typedef struct string {
            const char * expected;
            const char * value;
        } string;


typedef struct result {
    const char *field;
    const char *type;
    union {
        struct uint8 uint8;
        struct uint32 uint32;
        struct boolean boolean;
        struct string string;
    } value;
} result;

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
extern bool openDoor();

extern bool getCards();
extern bool getCard();
extern bool getCardByIndex();
extern bool putCard();
extern bool deleteCard();
extern bool deleteCards();

extern bool getEventIndex();
extern bool setEventIndex();
extern bool getEvent();
extern bool recordSpecialEvents();

extern bool getTimeProfile();
extern bool setTimeProfile();

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_NUMBER;
extern const uint32_t CARD_INDEX;
extern const uint32_t EVENT_INDEX;
extern const uint8_t DOOR;
extern const uint8_t PROFILE_ID;

extern bool evaluate(const char *, int, const result[]);


