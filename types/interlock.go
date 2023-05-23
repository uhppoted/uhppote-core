package types

type Interlock uint8

const (
	NoInterlock    Interlock = 0x00 // disabled
	Interlock12_34 Interlock = 0x01 // reader (1,2) and (3,4)
	Interlock13_24 Interlock = 0x02 // reader (1,3) and (2,4)
	Interlock123   Interlock = 0x03 // reader (1,2,3)
	Interlock1234  Interlock = 0x04 // reader (1,2,3,4)
)

func (i Interlock) String() string {
	return [...]string{"disabled", "1&2,3&4", "1&3,2&4", "1&2&3", "1&2&3&4"}[i]
}
