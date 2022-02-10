# CHANGELOG

## [Unreleased]
### Added

### Changed
1. Added -trimpath to build to anonymize local folders
2. Reworked types.Date to use zero value as invalid
3. Reworked types.DateTime to use zero value as invalid

### Removed


## [v0.7.2] - 2022-01-26

### Changed
1. Retyped UHPPOTE.BindAddress as types.BindAddr
2. Retyped UHPPOTE.BroadcastAddress as types.BroadcastAddr
3. Retyped UHPPOTE.ListenAddress as types.ListenAddr
4. Removed rollover (ref. https://github.com/uhppoted/uhppote-cli/issues/7)


## v0.7.1

### Added
1. Implemented `clear-task-list`
2. Implemented `add-task`
3. Implemented `refresh-task-list`

### Changed
1. DateTime Stringer implementation reworked to not take pointer receiver
2. `encoding/conf` moved to `uhppoted-lib`


## v0.7.0

### Added
2. Implemented `get-time-profile`
3. Implemented `set-time-profile`

### Changed
1. Rearchitected UHPPOTE implementation to use internal driver to send and receive UDP packets
