#pragma once

extern int getDevices(int argc, char **argv);
extern int getDevice(int argc, char **argv);
extern int setAddress(int argc, char **argv);
extern int getStatus(int argc, char **argv);
extern int getTime(int argc, char **argv);
extern int setTime(int argc, char **argv);
extern int getListener(int argc, char **argv);
extern int setListener(int argc, char **argv);
extern int getDoorControl(int argc, char **argv);
extern int setDoorControl(int argc, char **argv);
extern int openDoor(int argc, char **argv);

extern int getCards(int argc, char **argv);
extern int getCard(int argc, char **argv);
extern int getCardByIndex(int argc, char **argv);
extern int putCard(int argc, char **argv);
extern int deleteCard(int argc, char **argv);
extern int deleteCards(int argc, char **argv);

extern int getEventIndex(int argc, char **argv);
extern int setEventIndex(int argc, char **argv);
extern int getEvent(int argc, char **argv);
extern int recordSpecialEvents(int argc, char **argv);

extern int getTimeProfile(int argc, char **argv);
extern int setTimeProfile(int argc, char **argv);
