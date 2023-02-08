package types

import (
	"bytes"
	"encoding/json"
	"testing"
)

func TestPINMarshalUT0311L0x(t *testing.T) {
	tests := []struct {
		pin      PIN
		expected []byte
	}{
		{0, []byte{0x00, 0x00, 0x00}},
		{1, []byte{0x00, 0x00, 0x01}},
		{12, []byte{0x00, 0x00, 0x12}},
		{123, []byte{0x00, 0x01, 0x23}},
		{1234, []byte{0x00, 0x12, 0x34}},
		{12345, []byte{0x01, 0x23, 0x45}},
		{123456, []byte{0x12, 0x34, 0x56}},
		{999999, []byte{0x99, 0x99, 0x99}},
		{1000000, []byte{0x00, 0x00, 0x00}},
	}

	for _, test := range tests {
		if b, err := test.pin.MarshalUT0311L0x(); err != nil {
			t.Fatalf("Error marshalling PIN %v (%v)", test.pin, err)
		} else if !bytes.Equal(b, test.expected) {
			t.Errorf("Incorrectly marshalled PIN '%v' - expected:%v, got:%v", test.pin, test.expected, b)
		}
	}
}

func TestPINUnMarshalUT0311L0x(t *testing.T) {
	tests := []struct {
		bytes    []byte
		expected PIN
	}{
		{[]byte{0x00, 0x00, 0x00}, 0},
		{[]byte{0x00, 0x00, 0x01}, 1},
		{[]byte{0x00, 0x00, 0x12}, 12},
		{[]byte{0x00, 0x01, 0x23}, 123},
		{[]byte{0x00, 0x12, 0x34}, 1234},
		{[]byte{0x01, 0x23, 0x45}, 12345},
		{[]byte{0x12, 0x34, 0x56}, 123456},
		{[]byte{0x99, 0x99, 0x99}, 999999},
	}

	for _, test := range tests {
		var pin PIN
		if p, err := pin.UnmarshalUT0311L0x(test.bytes); err != nil {
			t.Fatalf("Error unmarshalling PIN %v (%v)", test.bytes, err)
		} else if pin != test.expected {
			t.Errorf("Incorrectly unmarshalled PIN '%v' - expected:%v, got:%v", test.bytes, test.expected, pin)
		} else if p == nil {
			t.Errorf("Incorrectly unmarshalled PIN '%v' - expected:%v, got:%v", test.bytes, test.expected, p)
		} else if v, ok := p.(*PIN); !ok || v == nil {
			t.Errorf("Incorrectly unmarshalled PIN '%v' - expected:%v, got:%v", test.bytes, test.expected, v)
		} else if *v != test.expected {
			t.Errorf("Incorrectly unmarshalled PIN '%v' - expected:%v, got:%T", test.bytes, test.expected, *v)
		}
	}
}

func TestPINMarshalJSON(t *testing.T) {
	tests := []struct {
		pin      PIN
		expected string
	}{
		{0, `""`},
		{1, `"1"`},
		{12, `"12"`},
		{123, `"123"`},
		{123456, `"123456"`},
		{999999, `"999999"`},
		{1000000, `""`},
	}

	for _, test := range tests {
		bytes, err := json.Marshal(test.pin)
		if err != nil {
			t.Fatalf("Error marshalling PIN %v (%v)", test.pin, err)
		} else if string(bytes) != test.expected {
			t.Errorf("Incorrectly marshalled PIN '%v' - expected:%v, got:%v", test.pin, test.expected, string(bytes))
		}
	}
}

func TestPINUnmarshalJSON(t *testing.T) {
	tests := []struct {
		json     string
		expected PIN
	}{
		{`""`, 0},
		{`"0"`, 0},
		{`"1"`, 1},
		{`"12"`, 12},
		{`"1234"`, 1234},
		{`"0123"`, 123},
		{`"123456"`, 123456},
		{`"999999"`, 999999},
	}

	for _, test := range tests {
		var pin PIN
		if err := json.Unmarshal([]byte(test.json), &pin); err != nil {
			t.Fatalf("Error unmarshalling PIN %v (%v)", test.json, err)
		} else if pin != test.expected {
			t.Errorf("Incorrectly unmarshalled PIN '%v' - expected:%v, got:%v", test.json, test.expected, pin)
		}
	}
}

func TestPINUnmarshalInvalidJSON(t *testing.T) {
	tests := []struct {
		json string
	}{
		{`"?"`},
		{`"1234567"`},
		{`"+1234"`},
		{`"01234a"`},
		{`"1000000"`},
	}

	for _, test := range tests {
		var pin PIN
		if err := json.Unmarshal([]byte(test.json), &pin); err == nil {
			t.Errorf("Error unmarshalling invalid PIN '%v' - expected error, got:%v", test.json, err)
		}
	}
}
