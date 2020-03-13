package uhppote

import (
	"github.com/uhppoted/uhppote-core/types"
)

type IDevice interface {
	GetCardsN(serialNumber uint32) (uint32, error)
}
