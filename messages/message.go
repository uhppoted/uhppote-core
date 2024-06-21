package messages

import (
	"fmt"
	"strings"
)

type Request interface {
}

type Response interface {
}

func dump(m []byte, prefix string) string {
	var b strings.Builder

	for ix := 0; ix < len(m); ix += 16 {
		chunk := m[ix:]

		fmt.Fprintf(&b, "%s%08x ", prefix, ix)
		for i := 0; i < 8 && i < len(chunk); i++ {
			fmt.Fprintf(&b, " %02x", chunk[i])
		}

		fmt.Fprintf(&b, " ")
		for i := 8; i < 16 && i < len(chunk); i++ {
			fmt.Fprintf(&b, " %02x", chunk[i])
		}

		fmt.Fprintln(&b)
	}

	return b.String()
}
