## CHANGELOG

### v0.7.2

1. Retyped UHPPOTE.BindAddress as types.BindAddr

### v0.7.1

1. Implemented `clear-task-list`
2. Implemented `add-task`
3. Implemented `refresh-task-list`
4. DateTime Stringer implementation reworked to not take pointer receiver
5. `encoding/conf` moved to `uhppoted-lib`

### v0.7.0

1. Rearchitected UHPPOTE implementation to use internal driver to send and receive UDP packets
2. Implemented `get-time-profile`
3. Implemented `set-time-profile`