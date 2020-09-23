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

// Type
// 0x00: no record
// 0x01: swipe record
// 0x02: door status, push button, device reboot, remote open door
// 0x03: warn record
// 0xFF: record index has been over-written

// Direction
// 0x01: in
// 0x02: out

// Reason
// 1	01	SwipePass	Swipe
// 2	02	(Reserved)
// 3	03	(Reserved)
// 4	04	(Reserved)
// 5	05	SwipeNOPass	Denied Access: PC Control
// 6	06	SwipeNOPass	Denied Access: No PRIVILEGE
// 7	07	SwipeNOPass	Denied Access: Wrong PASSWORD
// 8	08	SwipeNOPass	Denied Access: AntiBack
// 9	09	SwipeNOPass	Denied Access: More Cards
// 10	0A	SwipeNOPass	Denied Access: First Card Open
// 11	0B	SwipeNOPass	Denied Access: Door Set NC
// 12	0C	SwipeNOPass	Denied Access: InterLock
// 13	0D	SwipeNOPass	Denied Access: Limited Times
// 14	0E	(Reserved)
// 15	0F	SwipeNOPass	Denied Access: Invalid Timezone
// 16	10	(Reserved)
// 17	11	(Reserved)
// 18	12	SwipeNOPass	Denied Access
// 19	13	(Reserved)
// 20	14	ValidEvent	Push Button
// 21	15	(Reserved)
// 22	16	(Reserved)
// 23	17	ValidEvent	Door Open
// 24	18	ValidEvent	Door Closed
// 25	19	ValidEvent	Super Password Open Door
// 26	1A	(Reserved)
// 27	1B	(Reserved)
// 28	1C	Warn	Controller Power On
// 29	1D	Warn	Controller Reset
// 30	1E	(Reserved)
// 31	1F	Warn	Push Button Invalid: Forced Lock
// 32	20	Warn	Push Button Invalid: Not On Line
// 33	21	Warn	Push Button Invalid: InterLock
// 34	22	Warn	Threat
// 35	23	(Reserved)
// 36	24	(Reserved)
// 37	25	Warn	Open too long
// 38	26	Warn	Forced Open
// 39	27	Warn	Fire
// 40	28	Warn	Forced Close
// 41	29	Warn	Guard Against Theft
// 42	2A	Warn	7*24Hour Zone
// 43	2B	Warn	Emergency Call
// 44	2C	RemoteOpen	Remote Open Door
// 45	2D	RemoteOpen	Remote Open Door By USB Reader
