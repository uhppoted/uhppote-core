package messages

import (
	"fmt"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
)

var requests = map[byte]func() Request{
	0x20: func() Request { return new(GetStatusRequest) },
	0x30: func() Request { return new(SetTimeRequest) },
	0x32: func() Request { return new(GetTimeRequest) },
	0x40: func() Request { return new(OpenDoorRequest) },
	0x50: func() Request { return new(PutCardRequest) },
	0x52: func() Request { return new(DeleteCardRequest) },
	0x54: func() Request { return new(DeleteCardsRequest) },
	0x58: func() Request { return new(GetCardsRequest) },
	0x5a: func() Request { return new(GetCardByIDRequest) },
	0x5c: func() Request { return new(GetCardByIndexRequest) },
	0x80: func() Request { return new(SetDoorControlStateRequest) },
	0x82: func() Request { return new(GetDoorControlStateRequest) },
	0x88: func() Request { return new(SetTimeProfileRequest) },
	0x8a: func() Request { return new(ClearTimeProfilesRequest) },
	0x8c: func() Request { return new(SetSuperPasswordsRequest) },
	0x8e: func() Request { return new(RecordSpecialEventsRequest) },
	0x90: func() Request { return new(SetListenerRequest) },
	0x92: func() Request { return new(GetListenerRequest) },
	0x94: func() Request { return new(GetDeviceRequest) },
	0x96: func() Request { return new(SetAddressRequest) },
	0x98: func() Request { return new(GetTimeProfileRequest) },
	0xa0: func() Request { return new(SetPCControlRequest) },
	0xa2: func() Request { return new(SetInterlockRequest) },
	0xa4: func() Request { return new(ActivateAccessKeypadsRequest) },
	0xa6: func() Request { return new(ClearTaskListRequest) },
	0xa8: func() Request { return new(AddTaskRequest) },
	0xaa: func() Request { return new(SetFirstCardRequest) },
	0xac: func() Request { return new(RefreshTaskListRequest) },
	0xb0: func() Request { return new(GetEventRequest) },
	0xb2: func() Request { return new(SetEventIndexRequest) },
	0xb4: func() Request { return new(GetEventIndexRequest) },
}

func UnmarshalRequest(bytes []byte) (Request, error) {
	if len(bytes) != 64 {
		return nil, fmt.Errorf("invalid message length %d", len(bytes))
	}

	if bytes[0] != 0x17 {
		return nil, fmt.Errorf("invalid message type 0x%02x", bytes[0])
	}

	f := requests[bytes[1]]
	if f == nil {
		return nil, fmt.Errorf("unknown message type 0x%02x", bytes[1])
	}

	response := f()
	if err := codec.Unmarshal(bytes, response); err != nil {
		return nil, err
	}

	return response, nil
}
