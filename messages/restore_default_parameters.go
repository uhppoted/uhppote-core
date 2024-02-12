package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

// Request message layout for 'restore default parameters' (function code 0xc8).
// Resets the controller to the manufacturer default configuration.
type RestoreDefaultParametersRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0xc8"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	MagicWord    uint32             `uhppote:"offset:8"`
}

// Response message layout for 'restore default parameters' (function code 0xc8).
type RestoreDefaultParametersResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0xc8"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	Succeeded    bool               `uhppote:"offset:8"`
}
