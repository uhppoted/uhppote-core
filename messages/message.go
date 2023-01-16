package messages

import (
	"encoding/hex"
	"regexp"
)

type Request interface {
}

type Response interface {
}

func dump(m []byte, prefix string) string {
	regex := regexp.MustCompile("(?m)^(.*)")

	return regex.ReplaceAllString(hex.Dump(m), prefix+"$1")
}
