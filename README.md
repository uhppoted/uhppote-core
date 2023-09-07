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

| *Version* | *Description*                                                                              |
| --------- | ------------------------------------------------------------------------------------------ |
| v0.8.6    | Adds activate-keypads function                                                             |
| v0.8.5    | Adds set-interlock function                                                                |
| v0.8.4    | Adds set-pc-control function and card keypad PIN support                                   |
| v0.8.3    | Maintenance release                                                                        |
| v0.8.2    | Bumping Go version to 1.19                                                                 |
| v0.8.1    | Maintenance release                                                                        |
| v0.8.0    | Fixed Date unmarshalling for ""                                                            |
| v0.7.3    | Reworked zero value for types.Date and types.DateTime                                      |
| v0.7.2    | Replaced event rollover with simple indexes to match controller behaviour                  |
| v0.7.1    | Added support for task list functions from the extended API                                |
| v0.7.0    | Added support for time profiles from the extended API                                      |
| v0.6.12   | Improved handling of concurrent requests and invalid responses                             |
| v0.6.10   | Bumped version to 0.6.10 for initial `uhppoted-app-wild-apricot` release                   |
| v0.6.8    | Improved internal support for UHPPOTE v6.62 firmware                                       |
| v0.6.7    | Implements `record-special-events` for enabling and disabling door events                  |
| v0.6.5    | Maintenance release for version compatibility with NodeRED module                          |
| v0.6.4    | Added support for uhppoted-app-sheets                                                      |
| v0.6.3    | Added support for MQTT ACL API                                                             |
| v0.6.2    | Added support for REST ACL API                                                             |
| v0.6.1    | Added support for CLI ACL commands                                                         |
| v0.6.0    | Added `IDevice` interface to support `uhppoted-api` ACL functionality                      |
| v0.5.1    | Initial release following restructuring into standalone Go *modules* and *git submodules*  |

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
- [`SetPCControl`](#setpccontrol)
- [`ActivateKeypads`](#activatekeypads)
- [`SetSuperPasswords`](#setsuperpasswords)
- [`SetInterlock`](#setinterlock)
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

NOTES: 
1. The UHPPOTE access controller has a weird behaviour around the PIN field. According to the SDK 
   documentation, valid PINs are in the range 0 to 999999. However the controller will accept a 
   PIN number out of that range and only keep the lower 7 nibbles of the 32-bit unsigned value.
   e.g:

   | PIN     | Hex value | Stored as (hex) | Retrieved as (hex) | Retrieved as (decimal) |
   |---------|-----------|-----------------|--------------------|------------------------|
   | 0       | 0x000000  | 0x000000        | 0x000000           | 0                      |
   | 999999  | 0x0f423f  | 0x0f423f        | 0x0f423f           | 999999                 |
   | 1000000 | 0x0f4240  | 0x000000        | 0x000000           | 0                      |
   | 1000001 | 0x0f4241  | 0x000000        | 0x000000           | 0                      |
   | 1048576 | 0x100000  | 0x000000        | 0x000000           | 0                      |
   | 1048577 | 0x100001  | 0x000000        | 0x000001           | 1                      |
   | 1999999 | 0x1E847F  | 0x0E847F        | 0x000001           | 951423                 |

   To avoid unexpected behaviour, the _uhppote-core_ `put-card` implementation returns an error
   if the PIN is out of range.

2. As of release v0.8.5, `put-card` validates the card number against the Wiegand-26 standard. The
   development release allows the invoking function to provide a list of allowed formats:
   - `none`
   - `any`
   - `Wiegand-26`


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

#### `SetPCControl`

Enables or disables remote host access control. 

If remote host access control is enabled, the access controller expects the host to communicate at least
once every 30 seconds otherwise it reverts to local control of access using the stored list of cards (the
communication is not required to be a 'set-pc-control' command - any command is sufficient). If the access
controller has reverted to local control because no message has been received from the host for more than
30 seconds, any subsequent communication from the remote host will re-establish remote control mode again.

#### `ActivateKeypads`

Enables or disables the reader access keypads.

#### `SetSuperPasswords`

Sets the _super_ passwords for a door. The _super_ passwords enable keypad access without a card and the
controllers provide for up to 4 _super_ passwords to be assigned individually to each door managed by a
controller.

#### `SetInterlock`

Sets the interlocking between doors, with support for the following operational modes:
- `disabled`
- `doors 1&2`
- `doors 3&4`
- `doors 1&2 and 3&4`
- `doors 1&2&3`
- `doors 1&2&3&4`

#### `Listen`

Establishes a 'listening' UDP connection to receive events from a controller (or multiple controller) and invokes
a callback function to process each received event.




