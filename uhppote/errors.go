package uhppote

import (
	"errors"
)

var ErrInvalidListenerAddress = errors.New("invalid listener address")
var ErrIncorrectController = errors.New("response from incorrect controller")
