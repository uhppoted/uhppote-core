package types

import "fmt"

type MsgType uint8

type Result struct {
	SerialNumber SerialNumber
	Succeeded    bool
}

func (r *Result) String() string {
	return fmt.Sprintf("%v %v", r.SerialNumber, r.Succeeded)
}
