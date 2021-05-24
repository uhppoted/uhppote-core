package uhppote

import (
	"fmt"
	"net"
	"time"
)

type udp struct {
	bindAddr net.UDPAddr
	debug    bool
}

func (u *udp) Broadcast(request []byte, addr *net.UDPAddr) ([][]byte, error) {
	u.debugf(fmt.Sprintf(" ... request\n%s\n", dump(request, " ...          ")), nil)

	bind := u.bindAddr
	if bind.Port != 0 {
		guard.Lock()
		defer guard.Unlock()
	}

	var connection *net.UDPConn
	if conn, err := net.ListenUDP("udp", &bind); err != nil {
		return nil, fmt.Errorf("Error creating UDP socket (%v)", err)
	} else if conn == nil {
		return nil, fmt.Errorf("open() created invalid UDP socket (%v)", conn)
	} else {
		connection = conn
	}

	defer connection.Close()

	if err := connection.SetWriteDeadline(time.Now().Add(5000 * time.Millisecond)); err != nil {
		return nil, fmt.Errorf("Failed to set UDP write timeout [%v]", err)
	}

	if N, err := connection.WriteToUDP(request, addr); err != nil {
		return nil, fmt.Errorf("Failed to write to UDP socket [%v]", err)
	} else {
		u.debugf(fmt.Sprintf(" ... sent %v bytes to %v\n", N, addr), nil)
	}

	var replies = make([][]byte, 0)
	var err error

	go func() {
		for {
			reply := make([]byte, 2048)

			if N, remote, errx := connection.ReadFromUDP(reply); errx != nil {
				err = errx
				return
			} else {
				replies = append(replies, reply[:N])

				u.debugf(fmt.Sprintf(" ... received %v bytes from %v\n%s", N, remote, dump(reply[:N], " ...          ")), nil)
			}
		}
	}()

	time.Sleep(2500 * time.Millisecond)

	return replies, err
}

func (u *udp) Send(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
	bind := u.bindAddr
	if bind.Port != 0 {
		guard.Lock()
		defer guard.Unlock()
	}

	var connection *net.UDPConn
	if conn, err := net.ListenUDP("udp", &bind); err != nil {
		return fmt.Errorf("Error creating UDP socket (%v)", err)
	} else if conn == nil {
		return fmt.Errorf("open() created invalid UDP socket (%v)", conn)
	} else {
		connection = conn
	}

	defer connection.Close()

	if err := connection.SetWriteDeadline(time.Now().Add(5000 * time.Millisecond)); err != nil {
		return fmt.Errorf("Failed to set UDP write timeout [%v]", err)
	}

	if N, err := connection.WriteToUDP(request, addr); err != nil {
		return fmt.Errorf("Failed to write to UDP socket [%v]", err)
	} else {
		u.debugf(fmt.Sprintf(" ... sent %v bytes to %v\n", N, addr), nil)
	}

	u.debugf(fmt.Sprintf(" ... request\n%s\n", dump(request, " ...          ")), nil)

	if handler == nil {
		return nil
	}

	received := make(chan error)
	timer := time.NewTimer(5 * time.Second)
	defer timer.Stop()

	go func() {
		received <- u.receive(connection, handler)
	}()

	select {
	case err := <-received:
		if err != nil {
			u.debugf(" ... receive error", err)
		}
		return err

	case <-timer.C:
		return fmt.Errorf("Timeout waiting for reply")
	}
}

func (u *udp) receive(c *net.UDPConn, handler func([]byte) bool) error {
	m := make([]byte, 2048)

	if err := c.SetReadDeadline(time.Now().Add(15000 * time.Millisecond)); err != nil {
		return fmt.Errorf("Failed to set UDP timeout [%v]", err)
	}

	for {
		N, remote, err := c.ReadFromUDP(m)
		if err != nil {
			return err
		}

		u.debugf(fmt.Sprintf(" ... received %v bytes from %v\n ... response\n%s", N, remote, dump(m[:N], " ...          ")), nil)

		bytes := m[:N]

		if handler(bytes) {
			return nil
		}
	}
}

func (u *udp) debugf(msg string, err error) {
	if u.debug {
		if err != nil {
			fmt.Printf("%v: %v\n", msg, err)
		} else {
			fmt.Printf("%v\n", msg)
		}
	}
}
