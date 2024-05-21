package uhppote

import (
	"syscall"
)

/*
 * MacOS platform specific implementation to set SO_REUSEADDR, SO_REUSEPORT and TCP_QUICKACK.
 */
func setSocketOptions(fd uintptr) error {
	if err := syscall.SetsockoptInt(int(fd), syscall.SOL_SOCKET, syscall.SO_REUSEADDR, 1); err != nil {
		return err
	}

	if err := syscall.SetsockoptInt(int(fd), syscall.SOL_SOCKET, syscall.SO_REUSEPORT, 1); err != nil {
		return err
	}

	// NTS: seems TCP_QUICKACK not supported on MacOS
	// if err := syscall.SetsockoptInt(int(fd), syscall.SOL_SOCKET, syscall.TCP_QUICKACK, 1); err != nil {
	// 	return err
	// }

	return nil
}
