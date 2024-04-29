package uhppote

import (
	"bytes"
	"fmt"
	"net"
	"testing"
	"time"

	"github.com/uhppoted/uhppote-core/types"
)

func TestPutCard(t *testing.T) {
	message := []byte{
		0x17, 0x50, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := struct {
		request []byte
		card    types.Card
	}{

		request: []byte{
			0x17, 0x50, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0xd4, 0x88, 0x5d, 0x00, 0x20, 0x23, 0x01, 0x01,
			0x20, 0x23, 0x12, 0x31, 0x01, 0x00, 0x1d, 0x01, 0x31, 0xd4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		},
		card: types.Card{
			CardNumber: 6129876,
			From:       types.ToDate(2023, time.January, 1),
			To:         types.ToDate(2023, time.December, 31),
			Doors: map[uint8]uint8{
				1: 1,
				2: 0,
				3: 29,
				4: 1,
			},
			PIN: 54321,
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) error {
				if !bytes.Equal(request, expected.request) {
					return fmt.Errorf("invalid put-card request\n   expected:%v\n   got:     %v", expected.request, request)
				} else {
					handler(message)
					return nil
				}
			},
		},
	}

	card := types.Card{
		CardNumber: 6129876,
		From:       types.ToDate(2023, time.January, 1),
		To:         types.ToDate(2023, time.December, 31),
		Doors: map[uint8]uint8{
			1: 1,
			2: 0,
			3: 29,
			4: 1,
		},
		PIN: 54321,
	}

	if ok, err := u.PutCard(423187757, card); err != nil {
		t.Fatalf("Unexpected error returned from PutCard (%v)", err)
	} else if !ok {
		t.Errorf("Expected 'ok' response from PutCard, got:%v", ok)
	}
}

func TestPutCardWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.PutCard(0, types.Card{})
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}

func TestIsWiegand26(t *testing.T) {
	tests := []struct {
		card uint32
		ok   bool
	}{
		{0, true},
		{65535, true},
		{65536, false},
		{99999, false},

		{100000, true},
		{165535, true},
		{165536, false},
		{199999, false},

		{25500000, true},
		{25565535, true},
		{25565536, false},
		{25599999, false},

		{25600000, false},
		{25665535, false},
		{25665536, false},
		{25699999, false},

		{99900000, false},
		{99965535, false},
		{99965536, false},
		{99999999, false},
	}

	for _, test := range tests {
		ok := isWiegand26(test.card)

		if ok != test.ok {
			t.Errorf("incorrect Wiegand-26 validation for %v - expected:%v, got:%v", test.card, test.ok, ok)
		}
	}
}
