# uhppote-core

Go API for low-level access to the UT0311-L0x* TCP/IP Wiegand access control boards. This module provides the device level
access for the *uhppote-cli*, *uhppoted-rest* and *uhppoted-mqtt* modules. 

Supported operating systems:
- Linux
- MacOS
- Windows

## Raison d'être

The manufacturer supplied application is 'Windows-only' and provides limited support for integration with other
systems.

## Releases

## Development

### Building from source

#### Dependencies

| *Dependency*                        | *Description*                                          |
| ----------------------------------- | ------------------------------------------------------ |

## API

- FindDevices
- FindDevice
- SetAddress
- GetStatus
- GetTime
- SetTime
- GetDoorControlState
- SetDoorControlState
- GetListener
- SetListener
- GetCards
- GetCardByIndex
- GetCardById
- PutCard
- DeleteCard
- GetEvent
- GetEventIndex
- SetEventIndex
- OpenDoor
- Listen

