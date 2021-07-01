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
| v0.7.1    | Added support for task list functions from the extended API                               |
| v0.7.0    | Added support for time profiles from the extended API                                     |
| v0.6.12   | Improved handling of concurrent requests and invalid responses                            |
| v0.6.10   | Bumped version to 0.6.10 for initial `uhppoted-app-wild-apricot` release                  |
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

Assuming you have `Go` and `make` installed:

```
git clone https://github.com/uhppoted/uhppote-core.git
cd uhppote-core
make build
```

If you prefer not to use `make`:
```
git clone https://github.com/uhppoted/uhppote-core.git
cd uhppote-core
mkdir bin
go build -o bin ./...
```

#### Dependencies

| *Dependency*                        | *Description*                                          |
| ----------------------------------- | ------------------------------------------------------ |
| golang.org/x/lint/golint            | Additional *lint* check for release builds             |

## API

- [`GetDevices`](#getdevices)
- [`GetDevice`](#getdevice)
- SetAddress
- GetListener
- SetListener
- GetTime
- SetTime
- GetDoorControlState
- SetDoorControlState
- RecordSpecialEvents
- GetStatus
- GetCards
- GetCardById
- GetCardByIndex
- PutCard
- DeleteCard
- DeleteCards
- GetTimeProfile
- SetTimeProfile
- ClearTimeProfiles
- [`ClearTaskList`](#cleartasklist)
- [`AddTask`](#addtask)
- [`RefreshTaskList`](#refreshtasklist)
- SetTaskList
- GetEvent
- GetEventIndex
- SetEventIndex
- OpenDoor
- Listen

Other than `GetDevices` API which specifically uses UDP broadcast to issue a request, the API will uses the configured controller IP address if possible, falling back to a broadcast request if necessary.

#### `GetDevices`

Retrieves a list of all UHPPOTE controllers that respond to a broadcast `get-device` request.

#### `GetDevice`

Retrieves the controller information for a specific UHPPOTE controller from the response to a `get-device` request. 

#### `ClearTaskList`

Clears the scheduled task list on a specific UHPPOTE controller, preparatory to using `AddTask` and `RefreshTask`. 

#### `AddTask`

Adds a scheduled task to the task list on a specific UHPPOTE controller. The task is not activated until `RefreshTaskList` is invoked. `ClearTaskList` should have been invoked prior to invoking a sequence of `AddTask` invocations to put the task list in a known state.

#### `RefreshTaskList`

Activates all tasks added by `AddTask`.



