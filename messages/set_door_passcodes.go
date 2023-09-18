package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

// Request message to set the supervisor passcodes assigned to a door.
// Passcoddes are in the range [0..999999], with 0 corresponding to
// 'no code'
type SetDoorPasscodesRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x8c"` // message type (0x8c)
	SerialNumber types.SerialNumber `uhppote:"offset:4"`   // controller serial number
	Door         uint8              `uhppote:"offset:8"`   // door [1..4]
	Passcode1    uint32             `uhppote:"offset:12"`  // passcode 1 [0..999999]
	Passcode2    uint32             `uhppote:"offset:16"`  // passcode 2 [0..999999]
	Passcode3    uint32             `uhppote:"offset:20"`  // passcode 3 [0..999999]
	Passcode4    uint32             `uhppote:"offset:24"`  // passcode 4 [0..999999]
}

// Response message to a request set the 'super' passwords assigned to a door.
type SetDoorPasscodesResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x8c"` // message type (0x8c)
	SerialNumber types.SerialNumber `uhppote:"offset:4"`   // controller serial number
	Succeeded    bool               `uhppote:"offset:8"`   // request succeeded/failed
}
