package uhppote

import (
	"encoding/binary"
	"fmt"
	"net"
	"net/netip"
	"os"
	"time"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
	"github.com/uhppoted/uhppote-core/types"
)

const (
	VERSION string = "v0.8.9" // common version number used across uhppoted ecosystem
)

type none struct{}

type driver interface {
	Broadcast(*net.UDPAddr, []byte) ([][]byte, error)
	BroadcastTo(*net.UDPAddr, []byte, func([]byte) bool) ([]byte, error)
	SendUDP(*net.UDPAddr, []byte) ([]byte, error)
	SendTCP(*net.TCPAddr, []byte) ([]byte, error)
	Listen(chan any, chan any, func([]byte)) error
}

type uhppote struct {
	broadcastAddr types.BroadcastAddr
	listenAddr    types.ListenAddr
	devices       map[uint32]Device
	debug         bool
	driver        driver
}

func NewUHPPOTE(
	bindAddr types.BindAddr,
	broadcastAddr types.BroadcastAddr,
	listenAddr types.ListenAddr,
	timeout time.Duration,
	devices []Device,
	debug bool) IUHPPOTE {

	uhppote := uhppote{
		broadcastAddr: broadcastAddr,
		listenAddr:    listenAddr,
		devices:       map[uint32]Device{},
		driver: &ut0311{
			bindAddr:   bindAddr.AddrPort,
			listenAddr: listenAddr.AddrPort,
			timeout:    timeout,
			debug:      debug,
		},
		debug: debug,
	}

	for _, device := range devices {
		uhppote.devices[device.DeviceID] = device.Clone()
	}

	return &uhppote
}

func (u *uhppote) DeviceList() map[uint32]Device {
	list := map[uint32]Device{}
	if u != nil {
		for k, v := range u.devices {
			list[k] = v
		}
	}

	return list
}

// Returns a list of netip.AddrPort on which the system is listening for controller events.
//
// The list is:
// - empty if the internal 'listen address' is not valid
// - a single value if the internal 'listen address' is explicitly specified
// - a list of all the IPv4 interfaces if the 'listen address' is INADDR_ANY
func (u *uhppote) ListenAddrList() []netip.AddrPort {
	list := []netip.AddrPort{}

	if u != nil && u.listenAddr.IsValid() {
		if !u.listenAddr.Addr().IsUnspecified() {
			list = append(list, u.listenAddr.AddrPort)
		} else if ifaces, err := net.Interfaces(); err == nil {
			port := u.listenAddr.Port()

			for _, i := range ifaces {
				if addrs, err := i.Addrs(); err == nil {
					for _, a := range addrs {
						switch v := a.(type) {
						case *net.IPNet:
							if v.IP.To4() != nil && i.Flags&net.FlagLoopback == 0 {
								if addr, ok := netip.AddrFromSlice(v.IP.To4()); ok {
									list = append(list, netip.AddrPortFrom(addr, port))
								}
							}
						}
					}
				}
			}
		}
	}

	return list
}

// Broadcasts the request as a UDP message and returns the replies collected within the configured
// time limit. Invalid responses are discarded without raising an error.
//
// Returns an error if the send or receive failed.
func (u *uhppote) broadcast(request, reply any) ([]any, error) {
	if m, err := codec.Marshal(request); err != nil {
		return nil, err
	} else if responses, err := u.udpBroadcast(m); err != nil {
		return nil, err
	} else {
		replies := []any{}

		for _, bytes := range responses {
			// ... discard invalid replies
			if len(bytes) != 64 {
				u.debugf(" ... receive error", fmt.Errorf("invalid message length - expected:%v, got:%v", 64, len(bytes)))
				continue
			}

			// ... discard unparseable replies
			v, err := codec.UnmarshalAs(bytes, reply)
			if err != nil {
				u.debugf(" ... receive error", err)
				continue
			}

			replies = append(replies, v)
		}

		return replies, nil
	}
}

// Sends a UDP message to a specific device and returns the decoded response.
//
// The internal implementation anticipates replies from more than one device because the
// request may be broadcast - only the reply that matches the serial number is returned.
//
// Returns an error if the send, receive or decoding failed or the controller serial number
// is invalid (0).
func sendto[T any](u *uhppote, serialNumber uint32, request any) (T, error) {
	var reply T

	if serialNumber == 0 {
		return reply, fmt.Errorf("invalid controller ID (%v)", serialNumber)
	}

	m, err := codec.Marshal(request)
	if err != nil {
		return reply, err
	}

	f := func() ([]byte, error) {
		if controller, ok := u.devices[serialNumber]; !ok {
			return u.udpBroadcastTo(serialNumber, m)
		} else if controller.Address == nil || !controller.Address.IsValid() || controller.Address.Addr() == netip.IPv4Unspecified() {
			return u.udpBroadcastTo(serialNumber, m)
		} else if controller.Protocol == "tcp" {
			return u.tcpSendTo(*controller.Address, m)
		} else {
			return u.udpSendTo(*controller.Address, m)
		}
	}

	if response, err := f(); err != nil {
		return reply, err
	} else if response == nil { // only for set-ip which doesn't return a response
		return reply, nil
	} else {
		// ... invalid reply?
		if len(response) != 64 {
			return reply, fmt.Errorf("invalid message length - expected:%v, got:%v", 64, len(response))
		}

		// ... reply with incorrect controller ID ?
		if ID := binary.LittleEndian.Uint32(response[4:8]); serialNumber != 0 && ID != serialNumber {
			return reply, fmt.Errorf("invalid controller ID - expected:%v, got:%v", serialNumber, ID)
		}

		// ... unparseable reply ?
		if v, err := codec.UnmarshalAs(response, reply); err != nil {
			return reply, err
		} else {
			return v.(T), nil
		}
	}
}

// Broadcasts a UDP request and returns all received replies.
func (u *uhppote) udpBroadcast(request []byte) ([][]byte, error) {
	addr := resolve(u.broadcastAddr)

	return u.driver.Broadcast(addr, request)
}

// Broadcasts the UDP request and returns the first valid reply to the request.
func (u *uhppote) udpBroadcastTo(serialNumber uint32, request []byte) ([]byte, error) {
	addr := resolve(u.broadcastAddr)

	handler := func(bytes []byte) bool {
		if len(bytes) != 64 {
			u.debugf(" ... receive error", fmt.Errorf("invalid message length - expected:%v, got:%v", 64, len(bytes)))
			return false
		}

		if deviceID := binary.LittleEndian.Uint32(bytes[4:8]); deviceID != serialNumber {
			u.debugf(" ... receive error", fmt.Errorf("invalid device ID - expected:%v, got:%v", serialNumber, deviceID))
			return false
		}

		return true
	}

	return u.driver.BroadcastTo(addr, request, handler)
}

// Sends a UDP message to a specific controller address.
func (u *uhppote) udpSendTo(address netip.AddrPort, request []byte) ([]byte, error) {
	dest := net.UDPAddrFromAddrPort(address)

	return u.driver.SendUDP(dest, request)
}

// Sends the request as a TCP message and returns the reply (if any).
func (u *uhppote) tcpSendTo(address netip.AddrPort, request []byte) ([]byte, error) {
	dest := net.TCPAddrFromAddrPort(address)

	return u.driver.SendTCP(dest, request)
}

func (u *uhppote) listen(p chan *event, q chan os.Signal, listener Listener) error {
	handler := func(bytes []byte) {
		// ... discard invalid events
		if len(bytes) != 64 {
			listener.OnError(fmt.Errorf("invalid message length - expected:%v, got:%v", 64, len(bytes)))
			return
		}

		// ... discard replies without a valid device ID
		if deviceID := binary.LittleEndian.Uint32(bytes[4:8]); deviceID == 0 {
			listener.OnError(fmt.Errorf("invalid device ID (%v)", deviceID))
			return
		}

		// .. discard unparseable messages
		e := event{}
		if err := codec.Unmarshal(bytes, &e); err != nil {
			listener.OnError(err)
			return
		}

		p <- &e
	}

	signal := make(chan any)
	closed := make(chan any)

	err := u.driver.Listen(signal, closed, handler)
	if err != nil {
		return err
	}

	listener.OnConnected()

	<-q
	close(signal)
	<-closed

	return nil
}

func resolve(address types.BroadcastAddr) *net.UDPAddr {
	if address.IsValid() {
		return net.UDPAddrFromAddrPort(address.AddrPort)
	}

	addr := net.UDPAddr{
		IP:   make(net.IP, net.IPv4len),
		Port: 60000,
		Zone: "",
	}

	copy(addr.IP, net.IPv4bcast)

	return &addr
}

func (u *uhppote) debugf(msg string, err error) {
	if u.debug {
		if err != nil {
			fmt.Printf("%v: %v\n", msg, err)
		} else {
			fmt.Printf("%v\n", msg)
		}
	}
}
