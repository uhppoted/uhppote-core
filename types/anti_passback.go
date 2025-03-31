package types

type AntiPassback uint8

const (
	Disabled     AntiPassback = 0x00 // disabled
	Readers12_34 AntiPassback = 0x01 // readers 1:2; 3:4 (independently)
	Readers13_24 AntiPassback = 0x02 // readers (1,3):(2,4)
	Readers1_23  AntiPassback = 0x03 // readers 1:(2,3)
	Readers1_234 AntiPassback = 0x04 // readers 1:(2,3,4)
)

func (a AntiPassback) String() string {
	switch a {
	case Disabled:
		return "disabled"
	case Readers12_34:
		return "(1:2);(3:4)"
	case Readers13_24:
		return "(1,3):(2,4)"
	case Readers1_23:
		return "1:(2,3)"
	case Readers1_234:
		return "1:(2,3,4)"
	}

	return ""
}
