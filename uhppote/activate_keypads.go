package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) ActivateKeypads(controllerID uint32, readers map[uint8]bool) (bool, error) {
	if controllerID == 0 {
		return false, fmt.Errorf("invalid controller ID (%v)", controllerID)
	}

	request := messages.ActivateAccessKeypadsRequest{
		SerialNumber: types.SerialNumber(controllerID),
		Reader1:      readers[1],
		Reader2:      readers[2],
		Reader3:      readers[3],
		Reader4:      readers[4],
	}

	if reply, err := u.sendTo(controllerID, request, messages.ActivateAccessKeypadsResponse{}); err != nil {
		return false, err
	} else {
		return reply.(messages.ActivateAccessKeypadsResponse).Succeeded, nil
	}
}
