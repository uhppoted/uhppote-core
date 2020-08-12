package messages

import (
	"github.com/uhppoted/uhppote-core/types"
)

type GetStatusRequest struct {
	MsgType      types.MsgType      `uhppote:"value:0x20"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
}

type GetStatusResponse struct {
	MsgType      types.MsgType      `uhppote:"value:0x20"`
	SerialNumber types.SerialNumber `uhppote:"offset:4"`
	EventIndex   uint32             `uhppote:"offset:8"`
	EventType    byte               `uhppote:"offset:12"`
	Granted      bool               `uhppote:"offset:13"`
	Door         byte               `uhppote:"offset:14"`
	Direction    uint8              `uhppote:"offset:15"`
	CardNumber   uint32             `uhppote:"offset:16"`
	Timestamp    types.DateTime     `uhppote:"offset:20"`
	Reason       uint8              `uhppote:"offset:27"`
	Door1State   bool               `uhppote:"offset:28"`
	Door2State   bool               `uhppote:"offset:29"`
	Door3State   bool               `uhppote:"offset:30"`
	Door4State   bool               `uhppote:"offset:31"`
	Door1Button  bool               `uhppote:"offset:32"`
	Door2Button  bool               `uhppote:"offset:33"`
	Door3Button  bool               `uhppote:"offset:34"`
	Door4Button  bool               `uhppote:"offset:35"`
	SystemError  uint8              `uhppote:"offset:36"`
	SystemDate   types.SystemDate   `uhppote:"offset:51"`
	SystemTime   types.SystemTime   `uhppote:"offset:37"`
	SequenceId   uint32             `uhppote:"offset:40"`
	SpecialInfo  uint8              `uhppote:"offset:48"`
	RelayState   uint8              `uhppote:"offset:49"` // bitmap (0=locked, 1=unlocked, 0000:all doors locked)
	InputState   uint8              `uhppote:"offset:50"` // bitmap (bit 0: force locked, bit 1: fire alarm)
}
