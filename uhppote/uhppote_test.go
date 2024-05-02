package uhppote

import (
	"fmt"
	"net"
	"net/netip"
	"reflect"
	"sync"
	"testing"
	"time"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
)

type stub struct {
	broadcast   func(*net.UDPAddr, []byte) ([][]byte, error)
	broadcastTo func(*net.UDPAddr, []byte, func([]byte) bool) ([]byte, error)
	sendudp     func(*net.UDPAddr, []byte) ([]byte, error)
	sendtcp     func(*net.TCPAddr, []byte) ([]byte, error)
}

func (d *stub) Broadcast(addr *net.UDPAddr, m []byte) ([][]byte, error) {
	return d.broadcast(addr, m)
}

func (d *stub) BroadcastTo(addr *net.UDPAddr, m []byte, f func([]byte) bool) ([]byte, error) {
	return d.broadcastTo(addr, m, f)
}

func (d *stub) SendUDP(addr *net.UDPAddr, m []byte) ([]byte, error) {
	return d.sendudp(addr, m)
}

func (d *stub) SendTCP(addr *net.TCPAddr, m []byte) ([]byte, error) {
	return d.sendtcp(addr, m)
}

func (d *stub) Listen(signal chan interface{}, done chan interface{}, handler func(bytes []byte)) error {
	return fmt.Errorf("NOT IMPLEMENTED")
}

var date = func(s string) *types.Date {
	d, _ := time.ParseInLocation("2006-01-02", s, time.Local)
	p := types.Date(d)
	return &p
}

func TestBroadcastAddressRequest(t *testing.T) {
	expected := messages.DeleteCardResponse{
		MsgType:      0x52,
		SerialNumber: 423187757,
		Succeeded:    true,
	}

	request := messages.DeleteCardRequest{
		SerialNumber: 423187757,
		CardNumber:   6154412,
	}

	bind := udpaddr("127.0.0.1:12345")
	u := uhppote{
		devices:       make(map[uint32]Device),
		debug:         false,
		broadcastAddr: udpaddr("127.0.0.1:60000"),
		driver: &ut0311{
			bindAddr: bind.AddrPort(),
			timeout:  5000 * time.Millisecond,
			debug:    false,
		},
	}

	closed := make(chan int)
	c := listen(423187757, "127.0.0.1:60000", 0*time.Millisecond, closed, t)

	if c != nil {
		defer func() {
			c.Close()
		}()
	}

	if reply, err := sendto[messages.DeleteCardResponse](&u, 423187757, request); err != nil {
		t.Fatalf("%v", err)
	} else if !reflect.DeepEqual(reply, expected) {
		t.Fatalf("Incorrect reply:\nExpected:\n%v\nReturned:\n%v", expected, reply)
	}

	c.Close()

	<-closed
}

func TestSequentialRequests(t *testing.T) {
	expected := []messages.DeleteCardResponse{
		{MsgType: 0x52, SerialNumber: 423187757, Succeeded: true},
		{MsgType: 0x52, SerialNumber: 757781324, Succeeded: true},
	}

	request := messages.DeleteCardRequest{
		SerialNumber: 1000002,
		CardNumber:   6154412,
	}

	bind := udpaddr("127.0.0.1:12345")
	u := uhppote{
		debug:         false,
		broadcastAddr: udpaddr("127.0.0.1:60000"),
		devices: map[uint32]Device{
			423187757: Device{
				Address: addrport("127.0.0.1:65001"),
				Doors:   []string{},
			},

			757781324: Device{
				Address: addrport("127.0.0.1:65002"),
				Doors:   []string{},
			},
		},
		driver: &ut0311{
			bindAddr: bind.AddrPort(),
			timeout:  5000 * time.Millisecond,
			debug:    false,
		},
	}

	closed := make(chan int)
	listening := []*net.UDPConn{
		listen(423187757, "127.0.0.1:65001", 0*time.Millisecond, closed, t),
		listen(757781324, "127.0.0.1:65002", 0*time.Millisecond, closed, t),
	}

	defer func() {
		for _, c := range listening {
			if c != nil {
				c.Close()
			}
		}
	}()

	if reply, err := sendto[messages.DeleteCardResponse](&u, 423187757, request); err != nil {
		t.Fatalf("%v", err)
	} else if !reflect.DeepEqual(reply, expected[0]) {
		t.Fatalf("Incorrect reply - expected:%v, got:%v", expected[0], reply)
	}

	if reply, err := sendto[messages.DeleteCardResponse](&u, 757781324, request); err != nil {
		t.Fatalf("%v", err)
	} else if !reflect.DeepEqual(reply, expected[1]) {
		t.Fatalf("Incorrect reply - expected:%v, got:%v", expected[1], reply)
	}

	for _, c := range listening {
		if c != nil {
			c.Close()
			<-closed
		}
	}
}

func TestConcurrentRequestsWithUnboundPort(t *testing.T) {
	expected := []messages.DeleteCardResponse{
		{MsgType: 0x52, SerialNumber: 423187757, Succeeded: true},
		{MsgType: 0x52, SerialNumber: 757781324, Succeeded: true},
	}

	request := messages.DeleteCardRequest{
		SerialNumber: 1000002,
		CardNumber:   6154412,
	}

	bind := udpaddr("127.0.0.1:0")
	u := uhppote{
		debug:         false,
		broadcastAddr: bind,
		devices: map[uint32]Device{
			423187757: Device{
				Address: addrport("127.0.0.1:65001"),
				Doors:   []string{},
			},

			757781324: Device{
				Address: addrport("127.0.0.1:65002"),
				Doors:   []string{},
			},
		},
		driver: &ut0311{
			bindAddr: bind.AddrPort(),
			timeout:  5000 * time.Millisecond,
			debug:    false,
		},
	}

	closed := make(chan int)
	listening := []*net.UDPConn{
		listen(423187757, "127.0.0.1:65001", 1500*time.Millisecond, closed, t),
		listen(757781324, "127.0.0.1:65002", 500*time.Millisecond, closed, t),
	}

	defer func() {
		for _, c := range listening {
			if c != nil {
				c.Close()
			}
		}
	}()

	var wg sync.WaitGroup
	wg.Add(2)

	go func() {
		defer wg.Done()

		if reply, err := sendto[messages.DeleteCardResponse](&u, 423187757, request); err != nil {
			t.Errorf("%v", err)
		} else if !reflect.DeepEqual(reply, expected[0]) {
			t.Errorf("Incorrect response:\nexpected:\n%v\ngot:\n%v", expected[0], reply)
		}
	}()

	go func() {
		defer wg.Done()

		time.Sleep(500 * time.Millisecond)

		if reply, err := sendto[messages.DeleteCardResponse](&u, 757781324, request); err != nil {
			t.Errorf("%v", err)
		} else if !reflect.DeepEqual(reply, expected[1]) {
			t.Errorf("Incorrect reply:\nexpected:\n%v\ngot:     \n%v", expected[1], reply)
		}
	}()

	wg.Wait()

	for _, c := range listening {
		if c != nil {
			c.Close()
			<-closed
		}
	}
}

func TestConcurrentRequestsWithBoundPort(t *testing.T) {
	expected := []messages.DeleteCardResponse{
		{MsgType: 0x52, SerialNumber: 423187757, Succeeded: true},
		{MsgType: 0x52, SerialNumber: 757781324, Succeeded: true},
	}

	request := messages.DeleteCardRequest{
		SerialNumber: 1000002,
		CardNumber:   6154412,
	}

	bind := udpaddr("127.0.0.1:12345")
	u := uhppote{
		debug:         false,
		broadcastAddr: udpaddr("127.0.0.1:60000"),
		devices: map[uint32]Device{
			423187757: Device{
				Address: addrport("127.0.0.1:65001"),
				Doors:   []string{},
			},

			757781324: Device{
				Address: addrport("127.0.0.1:65002"),
				Doors:   []string{},
			},
		},
		driver: &ut0311{
			bindAddr: bind.AddrPort(),
			timeout:  5000 * time.Millisecond,
			debug:    false,
		},
	}

	closed := make(chan int)
	listening := []*net.UDPConn{
		listen(423187757, "127.0.0.1:65001", 1000*time.Millisecond, closed, t),
		listen(757781324, "127.0.0.1:65002", 500*time.Millisecond, closed, t),
	}

	defer func() {
		for _, c := range listening {
			if c != nil {
				c.Close()
			}
		}
	}()

	var wg sync.WaitGroup
	wg.Add(2)

	go func() {
		defer wg.Done()

		if reply, err := sendto[messages.DeleteCardResponse](&u, 423187757, request); err != nil {
			t.Errorf("%v", err)
		} else if !reflect.DeepEqual(reply, expected[0]) {
			t.Errorf("Incorrect reply:\nExpected:\n%v\nReturned:\n%v", expected, reply)
		}
	}()

	go func() {
		defer wg.Done()

		time.Sleep(500 * time.Millisecond)

		if reply, err := sendto[messages.DeleteCardResponse](&u, 757781324, request); err != nil {
			t.Errorf("%v", err)
		} else if !reflect.DeepEqual(reply, expected[1]) {
			t.Errorf("Incorrect reply:\nExpected:\n%v\nReturned:\n%v", expected[1], reply)
		}
	}()

	wg.Wait()

	for _, c := range listening {
		if c != nil {
			c.Close()
			<-closed
		}
	}
}

func listen(deviceID uint32, address string, delay time.Duration, closed chan int, t *testing.T) *net.UDPConn {
	addr, err := net.ResolveUDPAddr("udp", address)
	if err != nil {
		t.Fatalf("Error setting up test UDP device: %v", err)
	}

	c, err := net.ListenUDP("udp", addr)
	if err != nil {
		t.Fatalf("%v", err)
	}

	go func() {
		m := make([]byte, 2048)

		for {
			_, remote, err := c.ReadFromUDP(m)
			if err != nil {
				t.Logf("%v", err)
				break
			}

			response := messages.DeleteCardResponse{
				SerialNumber: types.SerialNumber(deviceID),
				Succeeded:    true,
			}

			m, err := codec.Marshal(response)
			if err != nil {
				t.Logf("%v", err)
				break
			}

			time.Sleep(delay)

			_, err = c.WriteTo(m, remote)
			if err != nil {
				t.Logf("%v", err)
				break
			}
		}

		closed <- 1
	}()

	return c
}

func udpaddr(address string) *net.UDPAddr {
	addr := netip.MustParseAddrPort(address)

	return net.UDPAddrFromAddrPort(addr)
}

func addrport(address string) *netip.AddrPort {
	addr := netip.MustParseAddrPort(address)

	return &addr
}
