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

type driver interface {
	Broadcast(*net.UDPAddr, []byte) ([][]byte, error)
	BroadcastTo(*net.UDPAddr, []byte, func([]byte) bool) ([]byte, error)
	Send([]byte, *net.UDPAddr, func([]byte) bool) error
	SendUDP(*net.UDPAddr, []byte) ([]byte, error)
	SendTCP(*net.TCPAddr, []byte) ([]byte, error)
	Listen(chan any, chan any, func([]byte)) error
}

type uhppote struct {
	broadcastAddr *net.UDPAddr
	listenAddr    *net.UDPAddr
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

	bind := bindAddr
	broadcast := net.UDPAddr(broadcastAddr)
	listen := net.UDPAddr(listenAddr)

	uhppote := uhppote{
		broadcastAddr: &broadcast,
		listenAddr:    &listen,
		devices:       map[uint32]Device{},
		driver: &ut0311{
			bindAddr:   bind.AddrPort(),
			listenAddr: listen,
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

func (u *uhppote) BroadcastAddr() *net.UDPAddr {
	if u != nil {
		return u.broadcastAddr
	}

	return nil
}

func (u *uhppote) ListenAddr() *net.UDPAddr {
	if u != nil {
		return u.listenAddr
	}

	return nil
}

/*
 * Broadcasts the request as a UDP message and returns the replies collected within the configured
 * time limit. Invalid responses are discarded without raising an error.
 *
 * Returns an error if the send or receive failed.
 */
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

/*
 * Sends a UDP message to a specific device and returns the decoded response.
 *
 * The internal implementation anticipates replies from more than one device because the
 * request may be broadcast - only the reply that matches the serial number is returned.
 *
 * Returns an error if the send, receive or decoding failed or the controller serial number
 * is invalid (0).
 */
func sendto[T any](u *uhppote, serialNumber uint32, request any) (T, error) {
	var v T

	if reply, err := u.sendTo(serialNumber, request, v); err != nil {
		return v, err
	} else {
		return reply.(T), err
	}
}

/*
 * Sends a UDP message to a specific device and returns the decoded response.
 *
 * The internal implementation anticipates replies from more than one device because the
 * request may be broadcast - only the reply that matches the serial number is returned.
 *
 * Returns an error if the send, receive or decoding failed or the controller serial number
 * is invalid (0).
 */
func (u *uhppote) sendTo(serialNumber uint32, request, reply any) (any, error) {
	if serialNumber == 0 {
		return nil, fmt.Errorf("invalid controller ID (%v)", serialNumber)
	}

	m, err := codec.Marshal(request)
	if err != nil {
		return nil, err
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
		return nil, err
	} else {
		// ... invalid reply?
		if len(response) != 64 {
			return nil, fmt.Errorf("invalid message length - expected:%v, got:%v", 64, len(response))
		}

		// ... reply with incorrect controller ID ?
		if ID := binary.LittleEndian.Uint32(response[4:8]); serialNumber != 0 && ID != serialNumber {
			return nil, fmt.Errorf("invalid controller ID - expected:%v, got:%v", serialNumber, ID)
		}

		// ... unparseable replies ?
		if v, err := codec.UnmarshalAs(response, reply); err != nil {
			return nil, err
		} else {
			return v, nil
		}
	}
}

/*
 * Broadcasts a UDP request and returns all received replies.
 */
func (u *uhppote) udpBroadcast(request []byte) ([][]byte, error) {
	return u.driver.Broadcast(u.broadcastAddress(), request)
}

/*
 * Broadcasts the UDP request and returns the first valid reply to the request.
 */
func (u *uhppote) udpBroadcastTo(serialNumber uint32, request []byte) ([]byte, error) {
	dest := u.broadcastAddress()

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

	return u.driver.BroadcastTo(dest, request, handler)
}

/*
 * Sends a UDP message to a specific controller address.
 */
func (u *uhppote) udpSendTo(address netip.AddrPort, request []byte) ([]byte, error) {
	dest := net.UDPAddrFromAddrPort(address)

	return u.driver.SendUDP(dest, request)
}

/*
 * Sends the request as a TCP message and returns the reply (if any).
 */
func (u *uhppote) tcpSendTo(address netip.AddrPort, request []byte) ([]byte, error) {
	dest := net.TCPAddrFromAddrPort(address)

	return u.driver.SendTCP(dest, request)
}

func (u *uhppote) send(serialNumber uint32, request, reply any) error {
	dest := u.broadcastAddress()
	if device, ok := u.devices[serialNumber]; ok {
		if device.Address != nil {
			dest = net.UDPAddrFromAddrPort(*device.Address)
		}
	}

	m, err := codec.Marshal(request)
	if err != nil {
		return err
	}

	var handler func([]byte) bool

	if reply != nil {
		handler = func(bytes []byte) bool {
			// ... discard invalid replies
			if len(bytes) != 64 {
				u.debugf(" ... receive error", fmt.Errorf("invalid message length - expected:%v, got:%v", 64, len(bytes)))
				return false
			}

			// ... discard replies without a valid device ID
			if deviceID := binary.LittleEndian.Uint32(bytes[4:8]); deviceID != serialNumber {
				u.debugf(" ... receive error", fmt.Errorf("invalid device ID - expected:%v, got:%v", serialNumber, deviceID))
				return false
			}

			// .. discard unparseable messages
			if err := codec.Unmarshal(bytes, reply); err != nil {
				u.debugf(" ... receive error", err)
				return false
			}

			return true
		}
	}

	if err := u.driver.Send(m, dest, handler); err != nil {
		return err
	}

	return nil
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

func (u *uhppote) broadcastAddress() *net.UDPAddr {
	if u.broadcastAddr != nil {
		return u.broadcastAddr
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
