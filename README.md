# uhppote-core

Go API for low-level access to the UT0311-L0x* TCP/IP Wiegand access control boards. This module provides the device level
access for the *uhppote-cli*, *uhppoted-rest* and *uhppoted-mqtt* modules. 

Supported operating systems:
- Linux
- MacOS
- Windows

## Releases

| *Version* | *Description*                                                                             |
| --------- | ----------------------------------------------------------------------------------------- |
| v0.6.3    | Added support for MQTT ACL API                                                            |
| v0.6.2    | Added support for REST ACL API                                                            |
| v0.6.1    | Added support for CLI ACL commands                                                        |
| v0.6.0    | Added `IDevice` interface to support `uhppoted-api` ACL functionality                     |
| v0.5.1    | Initial release following restructuring into standalone Go *modules* and *git submodules* |

## Development

### Building from source

#### Dependencies

| *Dependency*                        | *Description*                                          |
| ----------------------------------- | ------------------------------------------------------ |
| golang.org/x/lint/golint            | Additional *lint* check for release builds             |

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

