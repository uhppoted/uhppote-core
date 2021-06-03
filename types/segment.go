package types

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

type Segments map[uint8]Segment

type Segment struct {
	Start HHmm `json:"start,omitempty"`
	End   HHmm `json:"end,omitempty"`
}

func (ss Segments) String() string {
	segments := []string{}
	for _, ix := range []uint8{1, 2, 3} {
		if s := fmt.Sprintf("%v", ss[ix]); s != "" {
			segments = append(segments, s)
		}
	}

	return strings.Join(segments, ",")
}

func (ss Segments) MarshalJSON() ([]byte, error) {
	segments := []Segment{}

	for _, id := range []uint8{1, 2, 3} {
		if s, ok := ss[id]; ok {
			segments = append(segments, s)
		}
	}

	return json.Marshal(segments)
}

func (ss *Segments) UnmarshalJSON(bytes []byte) error {
	segments := []Segment{}

	err := json.Unmarshal(bytes, &segments)
	if err != nil {
		return err
	}

	if ss != nil {
		for ix, v := range segments {
			id := uint8(ix + 1)
			if id >= 1 && id <= 3 {
				(*ss)[id] = v
			}
		}
	}

	return nil
}

func (s Segment) String() string {
	start := time.Time(s.Start)
	end := time.Time(s.End)

	if start.Hour() == 0 && start.Minute() == 0 && end.Hour() == 0 && end.Minute() == 0 {
		return ""
	}

	return fmt.Sprintf("%v-%v", s.Start, s.End)
}
