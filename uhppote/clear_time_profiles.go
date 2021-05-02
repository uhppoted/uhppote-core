package uhppote

import (
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

func (u *UHPPOTE) ClearTimeProfiles(serialNumber uint32) (bool, error) {
	driver := iuhppote(u)
	if u.driver != nil {
		driver = u.driver
	}

	request := messages.ClearTimeProfilesRequest{
		SerialNumber: types.SerialNumber(serialNumber),
		MagicWord:    0x55aaaa55,
	}

	reply := messages.ClearTimeProfilesResponse{}

	err := driver.Send(serialNumber, request, &reply)
	if err != nil {
		return false, err
	}

	return reply.Succeeded, nil
}
