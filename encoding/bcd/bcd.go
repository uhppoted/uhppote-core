/*
 * This package provides functions for encoding and decoding between strings and Binary-Coded Decimal (BCD) format.
 */
package bcd

import (
	"fmt"
	"strings"
)

/*
 * Encode takes a string `s` containing only numeric characters ('0' to '9') and encodes it to BCD, prepending a
 * leading zero if necessary. Returns an error if the input string contains any non-numeric characters.
 *
 * The function returns a pointer to a newly allocated slice containing the BCD encoded bytes or a `nil` result
 * and an error if the input string cannot be encoded.
 */
func Encode(s string) (*[]byte, error) {
	N := (len(s) + 1) / 2
	bytes := make([]byte, N)
	ix := len(s) % 2

	for _, ch := range s {
		b := byte(0x00)

		switch ch {
		case '0':
			b = 0x00
		case '1':
			b = 0x01
		case '2':
			b = 0x02
		case '3':
			b = 0x03
		case '4':
			b = 0x04
		case '5':
			b = 0x05
		case '6':
			b = 0x06
		case '7':
			b = 0x07
		case '8':
			b = 0x08
		case '9':
			b = 0x09
		default:
			return nil, fmt.Errorf("invalid numeric string '%s'", s)
		}

		bytes[ix/2] *= 16
		bytes[ix/2] += b
		ix += 1
	}

	return &bytes, nil
}

/*
 * Decode takes a BCD encoded slice of bytes and decodes it to a numeric string, returning an error if the input
 * contains invalid BCD values.
 *
 * The function returns the numeric string or an empty string and an error.
 */
func Decode(bytes []byte) (string, error) {
	var s strings.Builder

	s.Grow(len(bytes) * 2)

	for _, b := range bytes {
		switch b & 0xf0 {
		case 0x00:
			s.WriteRune('0')
		case 0x10:
			s.WriteRune('1')
		case 0x20:
			s.WriteRune('2')
		case 0x30:
			s.WriteRune('3')
		case 0x40:
			s.WriteRune('4')
		case 0x50:
			s.WriteRune('5')
		case 0x60:
			s.WriteRune('6')
		case 0x70:
			s.WriteRune('7')
		case 0x80:
			s.WriteRune('8')
		case 0x90:
			s.WriteRune('9')
		default:
			return "", fmt.Errorf("invalid BCD number: '%x'", bytes)
		}

		switch b & 0x0f {
		case 0x00:
			s.WriteRune('0')
		case 0x01:
			s.WriteRune('1')
		case 0x02:
			s.WriteRune('2')
		case 0x03:
			s.WriteRune('3')
		case 0x04:
			s.WriteRune('4')
		case 0x05:
			s.WriteRune('5')
		case 0x06:
			s.WriteRune('6')
		case 0x07:
			s.WriteRune('7')
		case 0x08:
			s.WriteRune('8')
		case 0x09:
			s.WriteRune('9')
		default:
			return "", fmt.Errorf("invalid BCD number: '%x'", bytes)
		}
	}

	return s.String(), nil
}
