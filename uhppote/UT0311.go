package uhppote

import (
	"fmt"
	"net"
	"net/netip"
	"sync"
	"syscall"
	"time"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
)

type ut0311 struct {
	bindAddr   netip.AddrPort
	listenAddr netip.AddrPort
	timeout    time.Duration
	debug      bool
}

var guard sync.Mutex
var NOTIMEOUT = time.Time{}

/*
 * Broadcasts a request to a UDP address and returns the replies collected within the duration of
 * the 'timeout' of the 'udp' struct.
 *
 * Returns an error if the UDP socket open, send or read failed.
 */
func (u *ut0311) Broadcast(addr *net.UDPAddr, request []byte) ([][]byte, error) {
	u.debugf(fmt.Sprintf(" ... request\n%s\n", codec.Dump(request, " ...          ")), nil)

	bind := net.UDPAddrFromAddrPort(u.bindAddr)

	if bind == nil {
		bind = &net.UDPAddr{
			IP:   net.IPv4(0, 0, 0, 0),
			Port: 0,
			Zone: "",
		}
	}

	if bind.Port != 0 {
		guard.Lock()
		defer guard.Unlock()
	}

	var connection *net.UDPConn
	if conn, err := net.ListenUDP("udp", bind); err != nil {
		return nil, fmt.Errorf("error creating UDP socket (%v)", err)
	} else if conn == nil {
		return nil, fmt.Errorf("open() created invalid UDP socket (%v)", conn)
	} else {
		connection = conn
	}

	defer connection.Close()

	if err := connection.SetWriteDeadline(time.Now().Add(1000 * time.Millisecond)); err != nil {
		return nil, fmt.Errorf("failed to set UDP write timeout [%v]", err)
	}

	if err := connection.SetReadDeadline(NOTIMEOUT); err != nil {
		return nil, fmt.Errorf("failed to set UDP read timeout [%v]", err)
	}

	if N, err := connection.WriteToUDP(request, addr); err != nil {
		return nil, fmt.Errorf("failed to write to UDP socket [%v]", err)
	} else {
		u.debugf(fmt.Sprintf(" ... sent %v bytes to %v (UDP)\n", N, addr), nil)
	}

	var replies = make([][]byte, 0)
	var err error

	// NTS: set-ip doesn't return a reply
	if request[1] != 0x96 {
		go func() {
			for {
				reply := make([]byte, 2048)

				if N, remote, errx := connection.ReadFromUDP(reply); errx != nil {
					err = errx
					return
				} else {
					replies = append(replies, reply[:N])

					u.debugf(fmt.Sprintf(" ... received %v bytes from %v (UDP)\n%s", N, remote, codec.Dump(reply[:N], " ...          ")), nil)
				}
			}
		}()
	}

	time.Sleep(u.timeout)

	return replies, err
}

/*
 * Broadcasts a request to a UDP address and returns the first valid reply.
 *
 * Returns an error if the UDP socket open, send or read failed.
 */
func (u *ut0311) BroadcastTo(addr *net.UDPAddr, request []byte, callback func([]byte) bool) ([]byte, error) {
	u.debugf(fmt.Sprintf(" ... request\n%s\n", codec.Dump(request, " ...          ")), nil)

	deadline := time.Now().Add(u.timeout)
	bind := net.UDPAddrFromAddrPort(u.bindAddr)

	if bind == nil {
		bind = &net.UDPAddr{
			IP:   net.IPv4(0, 0, 0, 0),
			Port: 0,
			Zone: "",
		}
	}

	if bind.Port != 0 {
		guard.Lock()
		defer guard.Unlock()
	}

	if connection, err := net.ListenUDP("udp", bind); err != nil {
		return nil, fmt.Errorf("error creating UDP socket (%v)", err)
	} else if connection == nil {
		return nil, fmt.Errorf("open() created invalid UDP socket (%v)", connection)
	} else {

		defer connection.Close()

		if err := connection.SetDeadline(deadline); err != nil {
			return nil, err
		}

		if N, err := connection.WriteToUDP(request, addr); err != nil {
			return nil, fmt.Errorf("failed to write to UDP socket [%v]", err)
		} else {
			u.debugf(fmt.Sprintf(" ... sent %v bytes to %v (UDP)\n", N, addr), nil)
		}

		// NTS: set-ip doesn't return a reply
		if request[1] == 0x96 {
			return nil, nil
		}

		for {
			reply := make([]byte, 2048)

			if N, remote, err := connection.ReadFromUDP(reply); err != nil {
				return nil, err
			} else if callback(reply[:N]) {
				u.debugf(fmt.Sprintf(" ... received %v bytes from %v (UDP)\n%s", N, remote, codec.Dump(reply[:N], " ...          ")), nil)

				return reply[:N], err
			}
		}
	}

}

/*
 * Opens a connected UDP socket to the destination address, sends the request and returns the reply
 * (if any).
 *
 * Returns an error if the UDP socket open, send or read failed or the read timed out, otherwise
 * returns a byte slice with the reply.
 */
func (u *ut0311) SendUDP(addr *net.UDPAddr, request []byte) ([]byte, error) {
	deadline := time.Now().Add(u.timeout)
	address := fmt.Sprintf("%v", addr)
	bind := net.UDPAddrFromAddrPort(u.bindAddr)

	if bind == nil {
		bind = &net.UDPAddr{
			IP:   net.IPv4(0, 0, 0, 0),
			Port: 0,
			Zone: "",
		}
	}

	if bind.Port != 0 {
		guard.Lock()
		defer guard.Unlock()
	}

	dialer := net.Dialer{
		Deadline:  deadline,
		LocalAddr: bind,
		Control: func(network, address string, connection syscall.RawConn) (err error) {
			var operr error

			f := func(fd uintptr) {
				operr = setSocketOptions(fd)
			}

			if err := connection.Control(f); err != nil {
				return err
			} else {
				return operr
			}
		},
	}

	if connection, err := dialer.Dial("udp4", address); err != nil {
		return nil, err
	} else if connection == nil {
		return nil, fmt.Errorf("invalid UDP socket (%v)", connection)
	} else {
		defer connection.Close()

		deadline := time.Now().Add(u.timeout)
		buffer := make([]byte, 1024)

		if err := connection.SetDeadline(deadline); err != nil {
			return nil, err
		}

		if N, err := connection.Write(request); err != nil {
			return nil, fmt.Errorf("failed to write to UDP socket [%v]", err)
		} else {
			u.debugf(fmt.Sprintf(" ... sent %v bytes to %v (UDP)", N, addr), nil)
			u.debugf(fmt.Sprintf(" ... request\n%s\n", codec.Dump(request, " ...          ")), nil)
		}

		// NTS: set-ip doesn't return a reply
		if request[1] == 0x96 {
			return nil, nil
		}

		if N, err := connection.Read(buffer); err != nil {
			u.debugf(" ... receive error", err)
			return nil, err
		} else {
			u.debugf(fmt.Sprintf(" ... received %v bytes from %v (UDP)\n ... response\n%s", N, addr, codec.Dump(buffer[:N], " ...          ")), nil)

			return buffer[0:N], nil
		}
	}
}

/*
 * Opens a TCP socket to the destination address, sends the request and returns the reply (if any).
 *
 * Returns an error if the TCP socket open, send or read failed or the read timed out, otherwise
 * returns a byte slice with the reply.
 */
func (u *ut0311) SendTCP(addr *net.TCPAddr, request []byte) ([]byte, error) {
	deadline := time.Now().Add(u.timeout)
	address := fmt.Sprintf("%v", addr)
	bind := net.TCPAddrFromAddrPort(u.bindAddr)

	if bind == nil {
		bind = &net.TCPAddr{
			IP:   net.IPv4(0, 0, 0, 0),
			Port: 0,
			Zone: "",
		}
	}

	if bind.Port != 0 {
		guard.Lock()
		defer guard.Unlock()
	}

	dialer := net.Dialer{
		Deadline:  deadline,
		LocalAddr: bind,
		Control: func(network, address string, connection syscall.RawConn) (err error) {
			var operr error

			f := func(fd uintptr) {
				operr = setSocketOptions(fd)
			}

			if err := connection.Control(f); err != nil {
				return err
			} else {
				return operr
			}
		},
	}

	if connection, err := dialer.Dial("tcp4", address); err != nil {
		return nil, err
	} else if connection == nil {
		return nil, fmt.Errorf("invalid TCP socket (%v)", connection)
	} else {
		defer connection.Close()

		buffer := make([]byte, 1024)

		if err := connection.SetDeadline(deadline); err != nil {
			return nil, err
		}

		if N, err := connection.Write(request); err != nil {
			return nil, fmt.Errorf("failed to write to TCP socket [%v]", err)
		} else {
			u.debugf(fmt.Sprintf(" ... sent %v bytes to %v (TCP)", N, connection.RemoteAddr()), nil)
			u.debugf(fmt.Sprintf(" ... request\n%s\n", codec.Dump(request, " ...          ")), nil)
		}

		// NTS: set-ip doesn't return a reply
		if request[1] == 0x96 {
			return nil, nil
		}

		if N, err := connection.Read(buffer); err != nil {
			u.debugf(" ... receive error", err)
			return nil, err
		} else {
			u.debugf(fmt.Sprintf(" ... received %v bytes from %v (TCP)\n ... response\n%s",
				N,
				connection.RemoteAddr(),
				codec.Dump(buffer[:N], " ...          ")),
				nil)

			return buffer[0:N], nil
		}
	}
}

func (u *ut0311) Listen(signal chan any, done chan any, callback func([]byte)) error {
	bind := net.UDPAddrFromAddrPort(u.listenAddr)
	if bind.Port == 0 {
		return fmt.Errorf("Listen requires a non-zero UDP port")
	}

	c, err := net.ListenUDP("udp", bind)
	if err != nil {
		return fmt.Errorf("error opening UDP listen socket (%v)", err)
	} else if c == nil {
		return fmt.Errorf("failed to open UDP socket (%v)", c)
	}

	closed := false

	go func() {
		<-signal
		closed = true
		c.Close()
	}()

	go func() {
		m := make([]byte, 2048)

		for {
			u.debugf(" ... listening", nil)

			N, remote, err := c.ReadFromUDP(m)
			if err != nil {
				if closed {
					u.debugf(" ... listen socket closed", nil)
					break
				}

				u.debugf("Error reading from UDP socket", err)
				continue
			}

			u.debugf(fmt.Sprintf(" ... received %v bytes from %v (UDP)\n ... response\n%s\n", N, remote, codec.Dump(m[:N], " ...          ")), nil)

			callback(m[:N])
		}

		close(done)
	}()

	return nil
}

func (u *ut0311) debugf(msg string, err error) {
	if u.debug {
		if err != nil {
			fmt.Printf("%v: %v\n", msg, err)
		} else {
			fmt.Printf("%v\n", msg)
		}
	}
}
