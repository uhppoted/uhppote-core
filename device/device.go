package device

import (
	"github.com/uhppoted/uhppote-core/types"
)

type IDevice interface {
	GetCardsN(deviceID uint32) (uint32, error)
	GetCardByIndex(deviceID, index uint32) (*types.Card, error)
	GetCardByIdN(deviceID, cardNumber uint32) (*types.Card, error)
	PutCardN(deviceID uint32, card types.Card) (bool, error)
	DeleteCard(deviceID uint32, cardNumber uint32) (bool, error)
}
