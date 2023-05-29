package types

type Interlock uint8

const (
	NoInterlock    Interlock = 0x00 // disabled
	Interlock12    Interlock = 0x01 // doors (1,2)
	Interlock34    Interlock = 0x02 // doors (3,4)
	Interlock12_34 Interlock = 0x03 // doors (1,2) and (3,4)
	Interlock123   Interlock = 0x04 // doors (1,2,3)
	Interlock1234  Interlock = 0x08 // doors (1,2,3,4)
)

func (i Interlock) String() string {
	switch i {
	case NoInterlock:
		return "disabled"
	case Interlock12:
		return "1&2"
	case Interlock34:
		return "3&4"
	case Interlock12_34:
		return "1&2,3&4"
	case Interlock123:
		return "1&2&3"
	case Interlock1234:
		return "1&2&3&4"
	}

	return ""
}
