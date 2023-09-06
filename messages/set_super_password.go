package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

// Request message to set the 'super' password assigned to a door.
// Passwords are in the range [0..999999], with 0 corresponding to
// 'no password'
type SetSuperPasswordRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x8c"` // message type (0x8c)
	SerialNumber types.SerialNumber `uhppote:"offset:4"`   // controller serial number
	Door         uint8              `uhppote:"offset:8"`   // door [1..4]
	Password1    uint32             `uhppote:"offset:12"`  // super password 1 [0..999999]
	Password2    uint32             `uhppote:"offset:16"`  // super password 2 [0..999999]
	Password3    uint32             `uhppote:"offset:20"`  // super password 3 [0..999999]
	Password4    uint32             `uhppote:"offset:24"`  // super password 4 [0..999999]
}

// Response message to a request set the 'super' password assigned to a door.
type SetSuperPasswordResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x8c"` // message type (0x8c)
	SerialNumber types.SerialNumber `uhppote:"offset:4"`   // controller serial number
	Succeeded    bool               `uhppote:"offset:8"`   // request succeeded/failed
}
