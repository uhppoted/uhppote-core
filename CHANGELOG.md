# CHANGELOG

## Unreleased


## [0.8.8](https://github.com/uhppoted/uhppote-core/releases/tag/v0.8.8) - 2024-03-26

### Added
1. Implemented `restore-default-parameters` function.

### Updated
1. Bumped Go version to 1.22


## [0.8.7](https://github.com/uhppoted/uhppote-core/releases/tag/v0.8.7) - 2023-12-01

### Added
1. Implemented `set-super-passwords` function.

### Updated
1. Reworked `nil` event pointers as zero values.


## [0.8.6](https://github.com/uhppoted/uhppote-core/releases/tag/v0.8.6) - 2023-08-30

### Added
1. Implemented `activate-keypads` function.

### Updated
1. Reworked `put-card` implementation to allow for validation against multiple card formats.


## [0.8.5](https://github.com/uhppoted/uhppote-core/releases/tag/v0.8.5) - 2023-06-13

### Added
1. `set-interlock` extended function.

### Updated
1. Reworkd `get-card-xxx` to use zero valued dates rather than pointers.
2. Reworkd card `From` and `To` dates to not be pointers.
3. Added check for valid Wiegand-26 card number to `put-card`


## [0.8.4](https://github.com/uhppoted/uhppote-core/releases/tag/v0.8.4) - 2023-03-17

### Added
1. `doc.go` package overview documentation.
2. `SetPCControl` request and response (https://github.com/uhppoted/uhppote-core/issues/7)
3. Added PIN field to get-card, get-card-by-index and put-card messages and card record struct
   (cf. https://github.com/uhppoted/uhppote-core/issues/8)

### Updated
1. Fixed initial round of _staticcheck_ lint errors and permanently added _staticcheck_ to
   CI build.
2. Fixed `Status` formatting to accomodate events with indices greater than 100000.
3. Reworked `get-status` and events to handle controller with uninitialised system date/time.


## [0.8.3](https://github.com/uhppoted/uhppote-core/releases/tag/v0.8.3) - 2022-12-16

1. Maintenance release for version compatibility.


## [0.8.2](https://github.com/uhppoted/uhppote-core/releases/tag/v0.8.2) - 2022-10-14

### Changed
1. Updated go.mod to Go 1.19.


## [0.8.1](https://github.com/uhppoted/uhppote-core/releases/tag/v0.8.1) - 2022-08-01

1. Maintenance release to update dependencies.


## [0.8.0](https://github.com/uhppoted/uhppote-core/releases/tag/v0.8.0) - 2022-07-01

### Changed
1. Date.UnmarshalJSON: unmarshalled "" to zero value


## [0.7.3](https://github.com/uhppoted/uhppote-core/releases/tag/v0.7.3) - 2022-06-01

### Changed
1. Added -trimpath to build to anonymize local folders
2. Reworked types.Date to use zero value as invalid
3. Reworked types.DateTime to use zero value as invalid
4. Added ModeUnknown for door control state


## [0.7.2](https://github.com/uhppoted/uhppote-core/releases/tag/v0.7.2) - 2022-01-26

### Changed
1. Retyped UHPPOTE.BindAddress as types.BindAddr
2. Retyped UHPPOTE.BroadcastAddress as types.BroadcastAddr
3. Retyped UHPPOTE.ListenAddress as types.ListenAddr
4. Removed rollover (ref. https://github.com/uhppoted/uhppote-cli/issues/7)


## [0.7.1](https://github.com/uhppoted/uhppote-core/releases/tag/v0.7.1)

### Added
1. Implemented `clear-task-list`
2. Implemented `add-task`
3. Implemented `refresh-task-list`

### Changed
1. DateTime Stringer implementation reworked to not take pointer receiver
2. `encoding/conf` moved to `uhppoted-lib`


## [0.7.0](https://github.com/uhppoted/uhppote-core/releases/tag/v0.7.0)

### Added
2. Implemented `get-time-profile`
3. Implemented `set-time-profile`

### Changed
1. Rearchitected UHPPOTE implementation to use internal driver to send and receive UDP packets

## [0.6.12](https//github.com/uhppoted/uhppote-core/releases/tag/v0.6.12)

Improved handling of concurrent requests and invalid responses

## [0.6.10](https//github.com/uhppoted/uhppote-core/releases/tag/v0.6.10)

Bumped version to 0.6.10 for initial `uhppoted-app-wild-apricot` release

## [0.6.8](https//github.com/uhppoted/uhppote-core/releases/tag/v0.6.8)

Improved internal support for UHPPOTE v6.62 firmware

## [0.6.7](https//github.com/uhppoted/uhppote-core/releases/tag/v0.6.7)

Implements `record-special-events` for enabling and disabling door events

## [0.6.5](https//github.com/uhppoted/uhppote-core/releases/tag/v0.6.5)

Maintenance release for version compatibility with NodeRED module

## [0.6.4](https//github.com/uhppoted/uhppote-core/releases/tag/v0.6.4)

Added support for uhppoted-app-sheets

## [0.6.3](https//github.com/uhppoted/uhppote-core/releases/tag/v0.6.3)

Added support for MQTT ACL API

## [0.6.2](https//github.com/uhppoted/uhppote-core/releases/tag/v0.6.2)

Added support for REST ACL API

## [v0.6.1](https//github.com/uhppoted/uhppote-core/releases/tag/v0.6.1)

Added support for CLI ACL commands

## [v0.6.0](https//github.com/uhppoted/uhppote-core/releases/tag/v0.6.0)

Added `IDevice` interface to support `uhppoted-api` ACL functionality

## [0.5.1](https//github.com/uhppoted/uhppote-core/releases/tag/v0.5.1)

Initial release following restructuring into standalone Go *modules* and *git submodules*


