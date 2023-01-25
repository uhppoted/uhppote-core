package uhppote

import (
	"net"
	"os"
	"time"

	"github.com/uhppoted/uhppote-core/types"
)

type IUHPPOTE interface {
	GetDevices() ([]types.Device, error)
	GetDevice(deviceID uint32) (*types.Device, error)

	SetAddress(deviceID uint32, address, mask, gateway net.IP) (*types.Result, error)
	GetListener(deviceID uint32) (*types.Listener, error)
	SetListener(deviceID uint32, address net.UDPAddr) (*types.Result, error)
	GetTime(deviceID uint32) (*types.Time, error)
	SetTime(deviceID uint32, datetime time.Time) (*types.Time, error)
	GetDoorControlState(deviceID uint32, door byte) (*types.DoorControlState, error)
	SetDoorControlState(deviceID uint32, door uint8, state types.ControlState, delay uint8) (*types.DoorControlState, error)
	RecordSpecialEvents(deviceID uint32, enable bool) (bool, error)

	GetStatus(deviceID uint32) (*types.Status, error)

	GetCards(deviceID uint32) (uint32, error)
	GetCardByIndex(deviceID, index uint32) (*types.Card, error)
	GetCardByID(deviceID, cardNumber uint32) (*types.Card, error)
	PutCard(deviceID uint32, card types.Card) (bool, error)
	DeleteCard(deviceID uint32, cardNumber uint32) (bool, error)
	DeleteCards(deviceID uint32) (bool, error)

	GetTimeProfile(deviceID uint32, profileID uint8) (*types.TimeProfile, error)
	SetTimeProfile(deviceID uint32, profile types.TimeProfile) (bool, error)
	ClearTimeProfiles(deviceID uint32) (bool, error)

	ClearTaskList(deviceID uint32) (bool, error)
	AddTask(deviceID uint32, task types.Task) (bool, error)
	RefreshTaskList(deviceID uint32) (bool, error)

	GetEvent(deviceID, index uint32) (*types.Event, error)
	GetEventIndex(deviceID uint32) (*types.EventIndex, error)
	SetEventIndex(deviceID, index uint32) (*types.EventIndexResult, error)
	Listen(listener Listener, q chan os.Signal) error

	OpenDoor(deviceID uint32, door uint8) (*types.Result, error)
	SetPCControl(deviceID uint32, enable bool) (bool, error)

	// TODO: REMOVE (interim functions used by health-check)
	DeviceList() map[uint32]Device
	ListenAddr() *net.UDPAddr
}
