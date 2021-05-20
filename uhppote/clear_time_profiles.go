package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *uhppote) ClearTimeProfiles(serialNumber uint32) (bool, error) {
	request := messages.ClearTimeProfilesRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		MagicWord:    0x55aaaa55,
	}

	reply := messages.ClearTimeProfilesResponse{}

	err := u.impl.Send(serialNumber, request, &reply)
	if err != nil {
		return false, err
	}

	return reply.Succeeded, nil
}
