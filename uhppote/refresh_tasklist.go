package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) RefreshTaskList(deviceID uint32) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("Invalid device ID (%v)", deviceID)
	}

	request := messages.RefreshTaskListRequest{
		SerialNumber: types.SerialNumber(deviceID),
		MagicWord:    0x55aaaa55,
	}

	reply := messages.RefreshTaskListResponse{}

	err := u.send(deviceID, request, &reply)
	if err != nil {
		return false, err
	}

	return reply.Refreshed, nil
}
