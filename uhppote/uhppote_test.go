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
	execute     func(uint32, interface{}, interface{}) error
	send        func(uint32, interface{}) (messages.Response, error)
	broadcast   func(interface{}, interface{}) error
	broadcastTo func(uint32, interface{}, interface{}) error

	devices       func() map[uint32]*Device
	broadcastAddr func() *net.UDPAddr
}

func (m *mock) Execute(deviceID uint32, request, reply interface{}) error {
	return m.execute(deviceID, request, reply)
}

func (m *mock) Send(deviceID uint32, request interface{}) (messages.Response, error) {
	return m.send(deviceID, request)
}

func (m *mock) BroadcastAddr() *net.UDPAddr {
	return m.broadcastAddr()
}

func (m *mock) DeviceList() map[uint32]*Device {
	return m.devices()
}

func (m *mock) Broadcast(request interface{}, replies interface{}) error {
	return m.broadcast(request, replies)
}

func (m *mock) DirectedBroadcast(deviceID uint32, request interface{}, replies interface{}) error {
	return m.broadcastTo(deviceID, request, replies)
}

var date = func(s string) *types.Date {
	d, _ := time.ParseInLocation("2006-01-02", s, time.Local)
	p := types.Date(d)
	return &p
}

func TestBroadcastAddressRequest(t *testing.T) {
	expected, _ := messages.UnmarshalResponse([]byte{
		0x17, 0x52, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	})

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

	reply, err := u.Send(423187757, request)

	if err != nil {
		t.Fatalf("%v", err)
	}

	if reply == nil {
		t.Fatalf("Invalid reply: %v", reply)
	}

	if response, ok := reply.(*messages.DeleteCardResponse); !ok {
		t.Fatalf("Incorrect reply type - expected:%T, got:%T", expected, reply)
	} else if !reflect.DeepEqual(response, expected) {
		t.Fatalf("Incorrect reply:\nExpected:\n%v\nReturned:\n%v", expected, reply)
	}

	c.Close()

	<-closed
}

func TestSequentialRequests(t *testing.T) {
	expected := make([]messages.Response, 2)
	expected[0], _ = messages.UnmarshalResponse([]byte{
		0x17, 0x52, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	})

	expected[1], _ = messages.UnmarshalResponse([]byte{0x17, 0x52, 0x00, 0x00, 0x4c, 0xd3, 0x2a, 0x2d, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	})

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

	if reply, err := u.Send(423187757, request); err != nil {
		t.Fatalf("%v", err)
	} else if reply == nil {
		t.Fatalf("Invalid reply: %v", reply)
	} else if response, ok := reply.(*messages.DeleteCardResponse); !ok {
		t.Fatalf("Incorrect reply type - expected:%T, got:%T", expected, reply)
	} else if !reflect.DeepEqual(response, expected[0]) {
		t.Fatalf("Incorrect reply - expected:%v, got:%v", expected[0], reply)
	}

	if reply, err := u.Send(757781324, request); err != nil {
		t.Fatalf("%v", err)
	} else if reply == nil {
		t.Fatalf("Invalid reply: %v", reply)
	} else if response, ok := reply.(*messages.DeleteCardResponse); !ok {
		t.Fatalf("Incorrect reply type - expected:%T, got:%T", expected, reply)
	} else if !reflect.DeepEqual(response, expected[1]) {
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
	expected := make([]messages.Response, 2)

	expected[0], _ = messages.UnmarshalResponse([]byte{
		0x17, 0x52, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	})

	expected[1], _ = messages.UnmarshalResponse([]byte{
		0x17, 0x52, 0x00, 0x00, 0x4c, 0xd3, 0x2a, 0x2d, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	})

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

		if reply, err := u.Send(423187757, request); err != nil {
			t.Errorf("%v", err)
		} else if reply == nil {
			t.Errorf("Invalid reply: %v", reply)
		} else if !reflect.DeepEqual(reply, expected[0]) {
			t.Errorf("Incorrect reply:\nExpected:\n%v\nReturned:\n%v", expected[0], reply)
		}
	}()

	go func() {
		defer wg.Done()

		time.Sleep(500 * time.Millisecond)

		if reply, err := u.Send(757781324, request); err != nil {
			t.Errorf("%v", err)
		} else if reply == nil {
			t.Errorf("Invalid reply: %v", reply)
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

func TestConcurrentRequestsWithBoundPort(t *testing.T) {
	t.Skip("SKIP - uhppote concurrency implementation is a work in progress")

	expected := [][]byte{
		{0x17, 0x52, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		},
		{0x17, 0x52, 0x00, 0x00, 0x4c, 0xd3, 0x2a, 0x2d, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		},
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

		if reply, err := u.Send(423187757, request); err != nil {
			t.Errorf("%v", err)
		} else if reply == nil {
			t.Errorf("Invalid reply: %v", reply)
		} else if !reflect.DeepEqual(reply, expected[0]) {
			t.Errorf("Incorrect reply:\nExpected:\n%v\nReturned:\n%v", expected, reply)
		}
	}()

	go func() {
		defer wg.Done()

		time.Sleep(500 * time.Millisecond)

		if reply, err := u.Send(757781324, request); err != nil {
			t.Errorf("%v", err)
		} else if reply == nil {
			t.Errorf("Invalid reply: %v", reply)
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

func resolve(address string, t *testing.T) *net.UDPAddr {
	addr, err := net.ResolveUDPAddr("udp", address)
	if err != nil {
		t.Fatalf("Error resolving UDP address '%s': %v", address, err)
	}

	return addr
}
