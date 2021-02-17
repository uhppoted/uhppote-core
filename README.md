![build](https://github.com/uhppoted/uhppote-core/workflows/build/badge.svg)

# uhppote-core

Go API for low-level access to the UT0311-L0x* TCP/IP Wiegand access control boards. This module implements the
device level interface used by *uhppote-cli*, *uhppoted-api*, *uhppoted-rest*, etc. 

Supported operating systems:
- Linux
- MacOS
- Windows
- RaspberryPi (ARM7)

## Releases

| *Version* | *Description*                                                                             |
| --------- | ----------------------------------------------------------------------------------------- |
| v0.6.11   | Bumped version to 0.6.11 for initial `uhppoted-app-wild-apricot` release                  |
| v0.6.8    | Improved internal support for UHPPOTE v6.62 firmware                                      |
| v0.6.7    | Implements `record-special-events` for enabling and disabling door events                 |
| v0.6.5    | Maintenance release for version compatibility with NodeRED module                         |
| v0.6.4    | Added support for uhppoted-app-sheets                                                     |
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
- RecordSpecialEvents
- GetEvent
- GetEventIndex
- SetEventIndex
- RecordSpecialEvents
- OpenDoor
- Listen

