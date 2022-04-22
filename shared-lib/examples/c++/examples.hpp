#pragma once

#include "../include/uhppoted.hpp"

extern int getDevices(uhppoted &, int argc, char **argv);
extern int getDevice(uhppoted &u, int argc, char **argv);
extern int setAddress(uhppoted &, int argc, char **argv);
extern int getStatus(uhppoted &, int argc, char **argv);
extern int getTime(uhppoted &, int argc, char **argv);
extern int setTime(uhppoted &u, int argc, char **argv);
extern int getListener(uhppoted &, int argc, char **argv);
extern int setListener(uhppoted &, int argc, char **argv);
extern int getDoorControl(uhppoted &, int argc, char **argv);
extern int setDoorControl(uhppoted &, int argc, char **argv);
extern int openDoor(uhppoted &, int argc, char **argv);

int getCards(uhppoted &u, int argc, char **argv);
int getCard(uhppoted &u, int argc, char **argv);
int getCardByIndex(uhppoted &u, int argc, char **argv);
int putCard(uhppoted &u, int argc, char **argv);
int deleteCard(uhppoted &u, int argc, char **argv);
int deleteCards(uhppoted &u, int argc, char **argv);

int getEventIndex(uhppoted &u, int argc, char **argv);
int setEventIndex(uhppoted &u, int argc, char **argv);
int getEvent(uhppoted &u, int argc, char **argv);
int recordSpecialEvents(uhppoted &u, int argc, char **argv);

int getTimeProfile(uhppoted &u, int argc, char **argv);
int setTimeProfile(uhppoted &u, int argc, char **argv);
