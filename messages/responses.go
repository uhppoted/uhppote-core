package messages

import (
	"fmt"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
)

var responses = map[byte]func() Response{
	0x20: func() Response { return new(GetStatusResponse) },
	0x30: func() Response { return new(SetTimeResponse) },
	0x32: func() Response { return new(GetTimeResponse) },
	0x40: func() Response { return new(OpenDoorResponse) },
	0x50: func() Response { return new(PutCardResponse) },
	0x52: func() Response { return new(DeleteCardResponse) },
	0x54: func() Response { return new(DeleteCardsResponse) },
	0x58: func() Response { return new(GetCardsResponse) },
	0x5a: func() Response { return new(GetCardByIDResponse) },
	0x5c: func() Response { return new(GetCardByIndexResponse) },
	0x80: func() Response { return new(SetDoorControlStateResponse) },
	0x82: func() Response { return new(GetDoorControlStateResponse) },
	0x84: func() Response { return new(SetAntiPassbackResponse) },
	0x86: func() Response { return new(GetAntiPassbackResponse) },
	0x88: func() Response { return new(SetTimeProfileResponse) },
	0x8a: func() Response { return new(ClearTimeProfilesResponse) },
	0x8c: func() Response { return new(SetDoorPasscodesResponse) },
	0x8e: func() Response { return new(RecordSpecialEventsResponse) },
	0x90: func() Response { return new(SetListenerResponse) },
	0x92: func() Response { return new(GetListenerResponse) },
	0x94: func() Response { return new(GetDeviceResponse) },
	0x98: func() Response { return new(GetTimeProfileResponse) },
	0xa0: func() Response { return new(SetPCControlResponse) },
	0xa2: func() Response { return new(SetInterlockResponse) },
	0xa4: func() Response { return new(ActivateAccessKeypadsResponse) },
	0xa6: func() Response { return new(ClearTaskListResponse) },
	0xa8: func() Response { return new(AddTaskResponse) },
	0xaa: func() Response { return new(SetFirstCardResponse) },
	0xac: func() Response { return new(RefreshTaskListResponse) },
	0xb0: func() Response { return new(GetEventResponse) },
	0xb2: func() Response { return new(SetEventIndexResponse) },
	0xb4: func() Response { return new(GetEventIndexResponse) },
	0xc8: func() Response { return new(RestoreDefaultParametersResponse) },
}

func UnmarshalResponse(bytes []byte) (Response, error) {
	if len(bytes) != 64 {
		return nil, fmt.Errorf("invalid message length %d", len(bytes))
	}

	if bytes[0] != 0x17 {
		return nil, fmt.Errorf("invalid protocol ID 0x%02X", bytes[0])
	}

	f := responses[bytes[1]]
	if f == nil {
		return nil, fmt.Errorf("unknown message type %02X", bytes[1])
	}

	response := f()
	if err := codec.Unmarshal(bytes, response); err != nil {
		return nil, err
	}

	return response, nil
}
