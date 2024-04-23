package uhppote

import (
	"net/netip"
	"reflect"
	"testing"
	"time"
)

func TestNewDevice(t *testing.T) {
	address := netip.MustParseAddrPort("192.168.1.100:60000")

	expected := Device{
		Name:     "Alpha",
		DeviceID: 405419896,
		Address:  &address,
		Doors: []string{
			"Gryffindor",
			"Hufflepuff",
			"Ravenclaw",
			"Slytherin",
		},
		TimeZone: time.Local,
		Protocol: "udp",
	}

	device := NewDevice("Alpha", 405419896, &address, []string{
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
	address := netip.MustParseAddrPort("192.168.1.100:60000")

	expected := Device{
		Name:     "Alpha",
		DeviceID: 405419896,
		Address:  &address,
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
		Address:  &address,
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
