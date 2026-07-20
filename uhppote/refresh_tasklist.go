package uhppote

import (
	"fmt"

	"codeberg.org/uhppoted/uhppoted-core/messages"
	"codeberg.org/uhppoted/uhppoted-core/types"
)

func (u *uhppote) RefreshTaskList(deviceID uint32) (bool, error) {
	if deviceID == 0 {
		return false, fmt.Errorf("invalid device ID (%v)", deviceID)
	}

	request := messages.RefreshTaskListRequest{
		SerialNumber: types.SerialNumber(deviceID),
		MagicWord:    0x55aaaa55,
	}

	if reply, err := sendto[messages.RefreshTaskListResponse](u, deviceID, request); err != nil {
		return false, err
	} else {
		return reply.Refreshed, nil
	}
}
