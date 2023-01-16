package uhppote

import (
	"fmt"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) ClearTaskList(deviceID uint32) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.ClearTaskListRequest{
		SerialNumber: types.SerialNumber(deviceID),
		MagicWord:    0x55aaaa55,
	}

	reply := messages.ClearTaskListResponse{}

	err := u.send(deviceID, request, &reply)
	if err != nil {
		return false, err
	}

	return reply.Succeeded, nil
}
