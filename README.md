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
| v0.8.3    | Maintenance release                                                                       |
| v0.8.2    | Bumping Go version to 1.19                                                                |
| v0.8.1    | Maintenance release                                                                       |
| v0.8.0    | Fixed Date unmarshalling for ""                                                           |
| v0.7.3    | Reworked zero value for types.Date and types.DateTime                                     |
| v0.7.2    | Replaced event rollover with simple indexes to match controller behaviour                 |
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

If you prefer not to build manually:
```
git clone https://github.com/uhppoted/uhppote-core.git
cd uhppote-core
mkdir bin
go build -trimpath -o bin ./...
```

#### Dependencies

| *Dependency*                        | *Description*                                          |
| ----------------------------------- | ------------------------------------------------------ |
|                                     |                                                        |

## API

- [`GetDevices`](#getdevices)
- [`GetDevice`](#getdevice)
- [`SetAddress`](#setaddress)
- [`GetListener`](#getlistener)
- [`SetListener`](#setlistener)
- [`GetTime`](#gettime)
- [`SetTime`](#settime)
- [`GetDoorControlState`](#getdoorcontrolstate)
- [`SetDoorControlState`](#setdoorcontrolstate)
- [`RecordSpecialEvents`](#recordspecialevents)
- [`GetStatus`](#getstatus)
- [`GetCards`](#getcards)
- [`GetCardById`](#getcardbyid)
- [`GetCardByIndex`](#getcardbyindex)
- [`PutCard`](#putcard)
- [`DeleteCard`](#deletecard)
- [`DeleteCards`](#deletecards)
- [`GetTimeProfile`](#gettimeprofile)
- [`SetTimeProfile`](#settimeprofile)
- [`ClearTimeProfiles`](#cleartimeprofiles)
- [`ClearTaskList`](#cleartasklist)
- [`AddTask`](#addtask)
- [`RefreshTaskList`](#refreshtasklist)
- [`GetEvent`](#getevent)
- [`GetEventIndex`](#geteventindex)
- [`SetEventIndex`](#seteventindex)
- [`OpenDoor`](#opendoor)
- [`Listen`](#listen)

Other than `GetDevices` API which specifically uses UDP broadcast to issue a request, the API will use the configured controller IP address if possible, falling back to a broadcast request if necessary.

#### `GetDevices`

Retrieves a list of all UHPPOTE controllers that respond to a broadcast `get-device` request.

#### `GetDevice`

Retrieves the controller information for a specific UHPPOTE controller from the response to a `get-device` request. 

#### `SetAddress`

Sets the IPv4 address, subnet mask and gateway address for a controller.

#### `GetListener`

Retrieves the IPv4 address of the host configured to receive events from the controller.

#### `SetListener`

Sets the IPv4 address of the host to receive events from the controller.

#### `GetTime`

Retrieves the controller date and time.

#### `SetTime`

Sets the controller date and time.

#### `GetDoorControlState`

Retrieves a door control state (`normally open`, `normally closed` or `controlled`) from the controller.

#### `SetDoorControlState`

Sets a door control state (`normally open`, `normally closed` or `controlled`) on the controller.

#### `RecordSpecialEvents`

Enables or disables the door and relay events on a controller. 

#### `GetStatus`

Retrieves the controller current status.

#### `GetCards`

Retrieves the number of cards stored on a controller.

#### `GetCardByID`

Retrieves a stored card's information using the card number.

#### `GetCardByIndex`

Retrieves a stored card's information using an index into the card list.

#### `PutCard`

Adds or updates a card record on the controller.

#### `DeleteCard`

Deletes a card record from the controller.

#### `DeleteCards`

Deletes all card records from the controller.

#### `GetTimeProfile`

Retrieves a numbered time profile from the controller.

#### `SetTimeProfile`

Adds or updates a numbered time profile on the controller.

#### `ClearTimeProfiles`

Deletes all stored time profiles from a controller.

#### `ClearTaskList`

Clears the scheduled task list on a specific UHPPOTE controller, preparatory to using `AddTask` and `RefreshTask`. 

#### `AddTask`

Adds a scheduled task to the task list on a specific UHPPOTE controller. The task is not activated until `RefreshTaskList` is invoked. `ClearTaskList` should have been invoked prior to invoking a sequence of `AddTask` invocations to put the task list in a known state.

#### `RefreshTaskList`

Activates all tasks added by `AddTask`.

#### `GetEvent`

Retrieves a single stored event (by index) from the controller.

#### `GetEventIndex`

Retrieves the current value of the controller event index (typically used as a marker for events
that have been retrieved).

#### `SetEventIndex`

Sets the current value of the controller event index (typically used to update the marker for events
that have been retrieved).

#### `OpenDoor`

Unlocks a door remotely.

#### `Listen`

Establishes a 'listening' UDP connection to receive events from a controller (or multiple controller) and invokes
a callback function to process each received event.




