package types

import (
	"encoding/json"
	"reflect"
	"testing"
)

func TestSegmentsMarshalJSON(t *testing.T) {
	expected := []byte(`[{"start":"08:30","end":"09:45"},{"start":"11:35","end":"14:15"},{"start":"15:10","end":"18:00"}]`)

	segments := Segments{
		1: Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
		2: Segment{Start: hhmm("11:35"), End: hhmm("14:15")},
		3: Segment{Start: hhmm("15:10"), End: hhmm("18:00")},
	}

	bytes, err := json.Marshal(segments)
	if err != nil {
		t.Fatalf("Error marshalling segments (%v)", err)
	}

	if !reflect.DeepEqual(bytes, expected) {
		t.Errorf("Incorrectly marshalled segments\n   expected: %v\n        got: %v", string(expected), string(bytes))
	}
}

func TestSegmentsMarshalJSONWithEmptySegments(t *testing.T) {
	expected := []byte(`[]`)

	segments := Segments{}

	bytes, err := json.Marshal(segments)
	if err != nil {
		t.Fatalf("Error marshalling segments (%v)", err)
	}

	if !reflect.DeepEqual(bytes, expected) {
		t.Errorf("Incorrectly marshalled segments\n   expected: %v\n        got: %v", string(expected), string(bytes))
	}
}

func TestSegmentsMarshalJSONWithUnusableSegments(t *testing.T) {
	expected := []byte(`[{"start":"08:30","end":"09:45"},{"start":"11:35","end":"14:15"},{"start":"15:10","end":"18:00"}]`)

	segments := Segments{
		0: Segment{Start: hhmm("01:00"), End: hhmm("02:00")},
		1: Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
		2: Segment{Start: hhmm("11:35"), End: hhmm("14:15")},
		3: Segment{Start: hhmm("15:10"), End: hhmm("18:00")},
		4: Segment{Start: hhmm("19:00"), End: hhmm("20:00")},
	}

	bytes, err := json.Marshal(segments)
	if err != nil {
		t.Fatalf("Error marshalling segments (%v)", err)
	}

	if !reflect.DeepEqual(bytes, expected) {
		t.Errorf("Incorrectly marshalled segments\n   expected: %v\n        got: %v", string(expected), string(bytes))
	}
}

func TestSegmentsMarshalJSONWithMissingSegment(t *testing.T) {
	expected := []byte(`[{"start":"08:30","end":"09:45"},{"start":"15:10","end":"18:00"}]`)

	segments := Segments{
		1: Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
		3: Segment{Start: hhmm("15:10"), End: hhmm("18:00")},
	}

	bytes, err := json.Marshal(segments)
	if err != nil {
		t.Fatalf("Error marshalling segments (%v)", err)
	}

	if !reflect.DeepEqual(bytes, expected) {
		t.Errorf("Incorrectly marshalled segments\n   expected: %v\n        got: %v", string(expected), string(bytes))
	}
}

func TestSegmentsMarshalJSONWithPartialSegments(t *testing.T) {
	expected := []byte(`[{},{"start":"11:35"},{"end":"18:00"}]`)

	segments := Segments{
		1: Segment{},
		2: Segment{Start: hhmm("11:35")},
		3: Segment{End: hhmm("18:00")},
	}

	bytes, err := json.Marshal(segments)
	if err != nil {
		t.Fatalf("Error marshalling segments (%v)", err)
	}

	if !reflect.DeepEqual(bytes, expected) {
		t.Errorf("Incorrectly marshalled segments\n   expected: %v\n        got: %v", string(expected), string(bytes))
	}
}

func TestSegmentsUnmarshalJSON(t *testing.T) {
	expected := Segments{
		1: Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
		2: Segment{Start: hhmm("11:35"), End: hhmm("14:15")},
		3: Segment{Start: hhmm("15:10"), End: hhmm("18:00")},
	}

	segments := Segments{}

	if err := json.Unmarshal([]byte(`[ {"start":"08:30", "end":"09:45"},{"start":"11:35","end":"14:15"},{"start":"15:10","end":"18:00"} ]`), &segments); err != nil {
		t.Fatalf("Error unmarshalling segments (%v)", err)
	}

	if !reflect.DeepEqual(segments, expected) {
		t.Errorf("Incorrectly unmarshalled segments - expected:%v, got:%v", expected, segments)
	}
}

func TestSegmentsUnmarshalJSONWithEmptyArray(t *testing.T) {
	expected := Segments{}

	segments := Segments{}

	if err := json.Unmarshal([]byte(`[]`), &segments); err != nil {
		t.Fatalf("Error unmarshalling segments (%v)", err)
	}

	if !reflect.DeepEqual(segments, expected) {
		t.Errorf("Incorrectly unmarshalled segments - expected:%v, got:%v", expected, segments)
	}
}

func TestSegmentsUnmarshalJSONWithMissingSegment(t *testing.T) {
	expected := Segments{
		1: Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
		2: Segment{Start: hhmm("15:10"), End: hhmm("18:00")},
	}

	segments := Segments{}

	if err := json.Unmarshal([]byte(`[ {"start":"08:30", "end":"09:45"},{"start":"15:10","end":"18:00"} ]`), &segments); err != nil {
		t.Fatalf("Error unmarshalling segments (%v)", err)
	}

	if !reflect.DeepEqual(segments, expected) {
		t.Errorf("Incorrectly unmarshalled segments - expected:%v, got:%v", expected, segments)
	}
}

func TestSegmentsUnmarshalJSONWithPartialSegment(t *testing.T) {
	expected := Segments{
		1: Segment{},
		2: Segment{Start: hhmm("11:35")},
		3: Segment{End: hhmm("18:00")},
	}

	segments := Segments{}

	if err := json.Unmarshal([]byte(`[ {},{"start":"11:35"},{"end":"18:00"} ]`), &segments); err != nil {
		t.Fatalf("Error unmarshalling segments (%v)", err)
	}

	if !reflect.DeepEqual(segments, expected) {
		t.Errorf("Incorrectly unmarshalled segments - expected:%v, got:%v", expected, segments)
	}
}
