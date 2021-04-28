package uhppote

import (
	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
	"net"
	"reflect"
	"sync"
	"testing"
	"time"
)

type mock struct {
	send        func(uint32, interface{}, interface{}) error
	broadcast   func(interface{}, interface{}) ([]interface{}, error)
	broadcastTo func(uint32, interface{}, interface{}) ([]interface{}, error)

	devices       func() map[uint32]*Device
	broadcastAddr func() *net.UDPAddr
}

func (m *mock) Send(deviceID uint32, request, reply interface{}) error {
	return m.send(deviceID, request, reply)
}

func (m *mock) BroadcastAddr() *net.UDPAddr {
	return m.broadcastAddr()
}

func (m *mock) DeviceList() map[uint32]*Device {
	return m.devices()
}

func (m *mock) Broadcast(request, reply interface{}) ([]interface{}, error) {
	return m.broadcast(request, reply)
}

func (m *mock) BroadcastTo(deviceID uint32, request, reply interface{}) ([]interface{}, error) {
	return m.broadcastTo(deviceID, request, reply)
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

	u := UHPPOTE{
		Devices:          make(map[uint32]*Device),
		Debug:            true,
		BindAddress:      resolve("127.0.0.1:12345", t),
		BroadcastAddress: resolve("127.0.0.1:60000", t),
	}

	closed := make(chan int)
	c := listen(423187757, "127.0.0.1:60000", 0*time.Millisecond, closed, t)

	if c != nil {
		defer func() {
			c.Close()
		}()
	}

	response := messages.DeleteCardResponse{}

	err := u.Send(423187757, request, &response)
	if err != nil {
		t.Fatalf("%v", err)
	}

	if !reflect.DeepEqual(response, expected) {
		t.Fatalf("Incorrect reply:\nExpected:\n%v\nReturned:\n%v", expected, response)
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

	u := UHPPOTE{
		Debug:            true,
		BindAddress:      resolve("127.0.0.1:12345", t),
		BroadcastAddress: resolve("127.0.0.1:60000", t),
		Devices: map[uint32]*Device{
			423187757: &Device{
				Address:  resolve("127.0.0.1:65001", t),
				Rollover: 100000,
				Doors:    []string{},
			},

			757781324: &Device{
				Address:  resolve("127.0.0.1:65002", t),
				Rollover: 100000,
				Doors:    []string{},
			},
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

	response := messages.DeleteCardResponse{}

	if err := u.Send(423187757, request, &response); err != nil {
		t.Fatalf("%v", err)
	} else if !reflect.DeepEqual(response, expected[0]) {
		t.Fatalf("Incorrect reply - expected:%v, got:%v", expected[0], response)
	}

	response = messages.DeleteCardResponse{}
	if err := u.Send(757781324, request, &response); err != nil {
		t.Fatalf("%v", err)
	} else if !reflect.DeepEqual(response, expected[1]) {
		t.Fatalf("Incorrect reply - expected:%v, got:%v", expected[1], response)
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

	u := UHPPOTE{
		Debug:            true,
		BindAddress:      resolve("127.0.0.1:0", t),
		BroadcastAddress: resolve("127.0.0.1:60000", t),
		Devices: map[uint32]*Device{
			423187757: &Device{
				Address:  resolve("127.0.0.1:65001", t),
				Rollover: 100000,
				Doors:    []string{},
			},

			757781324: &Device{
				Address:  resolve("127.0.0.1:65002", t),
				Rollover: 100000,
				Doors:    []string{},
			},
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

		response := messages.DeleteCardResponse{}

		if err := u.Send(423187757, request, &response); err != nil {
			t.Errorf("%v", err)
		} else if !reflect.DeepEqual(response, expected[0]) {
			t.Errorf("Incorrect response:\nexpected:\n%v\ngot:\n%v", expected[0], response)
		}
	}()

	go func() {
		defer wg.Done()

		time.Sleep(500 * time.Millisecond)

		response := messages.DeleteCardResponse{}

		if err := u.Send(757781324, request, &response); err != nil {
			t.Errorf("%v", err)
		} else if !reflect.DeepEqual(response, expected[1]) {
			t.Errorf("Incorrect reply:\nexpected:\n%v\ngot:     \n%v", expected[1], response)
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

	u := UHPPOTE{
		Debug:            true,
		BindAddress:      resolve("127.0.0.1:12345", t),
		BroadcastAddress: resolve("127.0.0.1:60000", t),
		Devices: map[uint32]*Device{
			423187757: &Device{
				Address:  resolve("127.0.0.1:65001", t),
				Rollover: 100000,
				Doors:    []string{},
			},

			757781324: &Device{
				Address:  resolve("127.0.0.1:65002", t),
				Rollover: 100000,
				Doors:    []string{},
			},
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

		response := messages.DeleteCardResponse{}

		if err := u.Send(423187757, request, &response); err != nil {
			t.Errorf("%v", err)
		} else if !reflect.DeepEqual(response, expected[0]) {
			t.Errorf("Incorrect reply:\nExpected:\n%v\nReturned:\n%v", expected, response)
		}
	}()

	go func() {
		defer wg.Done()

		time.Sleep(500 * time.Millisecond)

		response := messages.DeleteCardResponse{}

		if err := u.Send(757781324, request, &response); err != nil {
			t.Errorf("%v", err)
		} else if !reflect.DeepEqual(response, expected[1]) {
			t.Errorf("Incorrect reply:\nExpected:\n%v\nReturned:\n%v", expected[1], response)
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

func resolve(address string, t *testing.T) *net.UDPAddr {
	addr, err := net.ResolveUDPAddr("udp", address)
	if err != nil {
		t.Fatalf("Error resolving UDP address '%s': %v", address, err)
	}

	return addr
}
