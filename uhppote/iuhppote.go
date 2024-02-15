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

	GetStatus(deviceID uint32) (*types.Status, error)

	GetCards(deviceID uint32) (uint32, error)
	GetCardByIndex(deviceID, index uint32) (*types.Card, error)
	GetCardByID(deviceID, cardNumber uint32) (*types.Card, error)
	PutCard(deviceID uint32, card types.Card, formats ...types.CardFormat) (bool, error)
	DeleteCard(deviceID uint32, cardNumber uint32) (bool, error)
	DeleteCards(deviceID uint32) (bool, error)

	GetTimeProfile(deviceID uint32, profileID uint8) (*types.TimeProfile, error)
	SetTimeProfile(deviceID uint32, profile types.TimeProfile) (bool, error)
	ClearTimeProfiles(deviceID uint32) (bool, error)

	ClearTaskList(deviceID uint32) (bool, error)
	AddTask(deviceID uint32, task types.Task) (bool, error)
	RefreshTaskList(deviceID uint32) (bool, error)

	// Sends a RecordSpecialEvents request to the designated controller, to enable or
	// disable door open, door closed and door button pressed events.
	//
	// Returns true if the controller 'record special events' flag was updated, false
	// if the request failed for any reason. Returns an error if the request could not
	// be sent or the response is invalid.
	RecordSpecialEvents(deviceID uint32, enable bool) (bool, error)

	GetEvent(deviceID, index uint32) (*types.Event, error)
	GetEventIndex(deviceID uint32) (*types.EventIndex, error)
	SetEventIndex(deviceID, index uint32) (*types.EventIndexResult, error)
	Listen(listener Listener, q chan os.Signal) error

	// Sends a SetDoorPasscodes request to the designated controller, to set the override
	// PIN codes for a door managed by the access controller.
	//
	// Each door may be individually assigned up to four passcodes, with valid codes being
	// in the range [1..999999]. The function uses the first four codes from the supplied
	// and invalid codes are replaced by 0 (disabled). If the supplied list contains less
	// than four codes, the remaining entries on the controller will be set to 0 (disabled).
	//
	// Returns true if the door passcodes were accepted by the access controller.
	SetDoorPasscodes(controllerID uint32, door uint8, passcodes ...uint32) (bool, error)

	OpenDoor(controllerID uint32, door uint8) (*types.Result, error) // remotely opens door

	// Sends a SetPCControl request to the designated controller, to enable or disable
	// remote host control of access.
	//
	// The access controller expects the host to communicate at least once every 30 seconds
	// otherwise it reverts to local control of access using the stored list of cards (the
	// communication is not required to be a 'set-pc-control' command - any command is sufficient).
	// If the access controller has reverted to local control because no message has been received
	// from the host for more than 30 seconds, any subsequent communication from the remote host
	// will re-establish remote control again.
	//
	// Returns true if the controller 'PC control' was successfully enabled (or disabled),
	// false if the request failed for any reason. Returns an error if the request could not
	// be sent or the response is invalid.
	SetPCControl(controllerID uint32, enable bool) (bool, error)

	SetInterlock(controllerID uint32, interlock types.Interlock) (bool, error) // sets door interlock mode
	ActivateKeypads(controllerID uint32, readers map[uint8]bool) (bool, error) // enables/disables reader keypads

	// Sends a RestoreDefaultParameters request to the designated controller, to reset it to the
	// manufacturer default configuration.
	//
	// Returns true if the controller was successfully reset, false if the request failed for any reason.
	// Returns an error if the request could not be sent or the response is invalid.
	RestoreDefaultParameters(controllerID uint32) (bool, error)

	// TODO: REMOVE (interim functions used by health-check)
	DeviceList() map[uint32]Device
	ListenAddr() *net.UDPAddr
}
