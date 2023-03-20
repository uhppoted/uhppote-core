# CHANGELOG

## Unreleased

### Updated
1. Reworkd `get-card-xxx` to use zero valued dates rather than pointers.
2. Reworkd card `From` and `To` dates to not be pointers.


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
