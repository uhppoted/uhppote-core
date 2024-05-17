package uhppote

import (
	"reflect"
	"testing"
	"time"

	"github.com/uhppoted/uhppote-core/types"
)

func TestNewDevice(t *testing.T) {
	expected := Device{
		Name:     "Alpha",
		DeviceID: 405419896,
		Address:  types.MustParseControllerAddr("192.168.1.100"),
		Doors: []string{
			"Gryffindor",
			"Hufflepuff",
			"Ravenclaw",
			"Slytherin",
		},
		TimeZone: time.Local,
		Protocol: "udp",
	}

	device := NewDevice("Alpha", 405419896, types.MustParseControllerAddr("192.168.1.100"), "udp", []string{
		"Gryffindor",
		"Hufflepuff",
		"Ravenclaw",
		"Slytherin",
	})

	if device == nil {
		t.Fatalf("error creating Device (%v)", device)
	}

	if !reflect.DeepEqual(*device, expected) {
		t.Errorf("incorrectly created Device\nexpected: %+v\ngot:      %+v", expected, *device)
	}
}

func TestDeviceClone(t *testing.T) {
	expected := Device{
		Name:     "Alpha",
		DeviceID: 405419896,
		Address:  types.MustParseControllerAddr("192.168.1.100"),
		Doors: []string{
			"Gryffindor",
			"Hufflepuff",
			"Ravenclaw",
			"Slytherin",
		},
		TimeZone: time.FixedZone("UTC-8", -8*60*60),
		Protocol: "udp",
	}

	device := Device{
		Name:     "Alpha",
		DeviceID: 405419896,
		Address:  types.MustParseControllerAddr("192.168.1.100"),
		Doors: []string{
			"Gryffindor",
			"Hufflepuff",
			"Ravenclaw",
			"Slytherin",
		},
		TimeZone: time.FixedZone("UTC-8", -8*60*60),
		Protocol: "udp",
	}

	cloned := device.Clone()

	if !reflect.DeepEqual(cloned, expected) {
		t.Errorf("incorrectly cloned Device\nexpected: %+v\ngot:      %+v", expected, cloned)
	}
}
