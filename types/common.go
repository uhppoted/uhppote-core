package types

import "fmt"

type SOM uint8
type MsgType uint8

type Result struct {
	SerialNumber SerialNumber
	Succeeded    bool
}

func (r *Result) String() string {
	return fmt.Sprintf("%v %v", r.SerialNumber, r.Succeeded)
}
