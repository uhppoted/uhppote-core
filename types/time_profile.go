package types

import ()

type TimeProfile struct {
	ProfileID       uint8 `json:"profile-id"`
	LinkedProfileID uint8 `json:"linked-profile-id"`
	From            *Date `json:"start-date"`
	To              *Date `json:"end-date"`

	Weekdays struct {
		Monday    bool `json:"monday"`
		Tuesday   bool `json:"tuesday"`
		Wednesday bool `json:"wednesday"`
		Thursday  bool `json:"thursday"`
		Friday    bool `json:"friday"`
		Saturday  bool `json:"saturday"`
		Sunday    bool `json:"sunday"`
	} `json:"weekdays"`

	Segments map[uint8]struct {
		Start *HHmm `json:"start"`
		End   *HHmm `json:"end"`
	} `json:"segments"`
}
