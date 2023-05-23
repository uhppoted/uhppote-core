package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) SetInterlock(controllerID uint32, interlock types.Interlock) (bool, error) {
	if controllerID == 0 {
		return false, fmt.Errorf("invalid controller ID (%v)", controllerID)
	}

	request := messages.SetInterlockRequest{
		SerialNumber: types.SerialNumber(controllerID),
		Interlock:    uint8(interlock),
	}

	reply := messages.SetInterlockResponse{}

	if err := u.send(controllerID, request, &reply); err != nil {
		return false, err
	} else {
		return reply.Succeeded, nil
	}
}
