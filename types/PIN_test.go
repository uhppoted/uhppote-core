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
		{1, []byte{0x01, 0x00, 0x00}},
		{12, []byte{0x0c, 0x00, 0x00}},
		{123, []byte{0x7b, 0x00, 0x00}},
		{1234, []byte{0xd2, 0x04, 0x00}},
		{12345, []byte{0x39, 0x30, 0x00}},
		{123456, []byte{0x40, 0xe2, 0x01}},
		{999999, []byte{0x3f, 0x42, 0x0f}},
	}

	for _, test := range tests {
		if b, err := test.pin.MarshalUT0311L0x(); err != nil {
			t.Fatalf("Error marshalling PIN %v (%v)", test.pin, err)
		} else if !bytes.Equal(b, test.expected) {
			t.Errorf("Incorrectly marshalled PIN '%v' - expected:%v, got:%v", test.pin, test.expected, b)
		}
	}
}

func TestInvalidPINMarshalUT0311L0x(t *testing.T) {
	tests := []struct {
		pin      PIN
		expected []byte
	}{
		{1000000, []byte{0x00, 0x00, 0x00}},
	}

	for _, test := range tests {
		if b, err := test.pin.MarshalUT0311L0x(); err == nil {
			t.Errorf("Incorrectly marshalled invalid PIN '%v' - expected error, got:%v", test.pin, err)
		} else if !bytes.Equal(b, test.expected) {
			t.Errorf("Incorrectly marshalled invalid PIN '%v' - expected:%v, got:%v", test.pin, test.expected, b)
		}
	}
}

func TestPINUnMarshalUT0311L0x(t *testing.T) {
	tests := []struct {
		bytes    []byte
		expected PIN
	}{
		{[]byte{0x00, 0x00, 0x00}, 0},
		{[]byte{0x01, 0x00, 0x00}, 1},
		{[]byte{0x0c, 0x00, 0x00}, 12},
		{[]byte{0x7b, 0x00, 0x00}, 123},
		{[]byte{0xd2, 0x04, 0x00}, 1234},
		{[]byte{0x39, 0x30, 0x00}, 12345},
		{[]byte{0x40, 0xe2, 0x01}, 123456},
		{[]byte{0x3f, 0x42, 0x0f}, 999999},
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

func TestInvalidPINUnMarshalUT0311L0x(t *testing.T) {
	tests := []struct {
		bytes []byte
	}{
		{[]byte{0x40, 0x42, 0x0f}},
	}

	for _, test := range tests {
		var pin PIN
		if p, err := pin.UnmarshalUT0311L0x(test.bytes); err == nil {
			t.Fatalf("Error unmarshalling invalid PIN %v - expected error, got: %v", test.bytes, err)
		} else if p != nil {
			t.Errorf("Incorrectly unmarshalled invalid PIN '%v' - expected:%v, got:%v", test.bytes, nil, p)
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
