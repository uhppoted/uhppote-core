package uhppote

type IDevice interface {
	GetCardsN(serialNumber uint32) (uint32, error)
}
