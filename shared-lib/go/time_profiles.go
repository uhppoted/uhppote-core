//go:build !debug && !tests

package main

import (
	"C"
	"fmt"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func getTimeProfile(uu uhppote.IUHPPOTE, deviceID uint32, profileID uint8) (*types.TimeProfile, error) {
	profile, err := uu.GetTimeProfile(deviceID, profileID)
	if err != nil {
		return nil, err
	} else if profile == nil {
		return nil, fmt.Errorf("%v: no response to get-time-profile %v", deviceID, profileID)
	} else {
		return profile, nil
	}
}

func setTimeProfile(uu uhppote.IUHPPOTE, deviceID uint32, profile types.TimeProfile) error {
	ok, err := uu.SetTimeProfile(deviceID, profile)
	if err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: set-time-profile failed for %v", deviceID, profile.ID)
	}

	return nil
}
