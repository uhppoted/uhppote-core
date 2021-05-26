package uhppote

import (
	"encoding/binary"
	"encoding/hex"
	"fmt"
	"net"
	"os"
	"regexp"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
)

var VERSION string = "v0.7.x"

type driver interface {
	Broadcast([]byte, *net.UDPAddr) ([][]byte, error)
	Send([]byte, *net.UDPAddr, func([]byte) bool) error
}

type uhppote struct {
	bindAddr      *net.UDPAddr
	broadcastAddr *net.UDPAddr
	listenAddr    *net.UDPAddr
	devices       map[uint32]Device
	debug         bool
	driver        driver
}

func NewUHPPOTE(bind, broadcast, listen net.UDPAddr, devices []Device, debug bool) IUHPPOTE {
	uhppote := uhppote{
		bindAddr:      &bind,
		broadcastAddr: &broadcast,
		listenAddr:    &listen,
		devices:       map[uint32]Device{},
		driver: &udp{
			bindAddr: bind,
			debug:    debug,
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

func (u *uhppote) broadcast(request, reply interface{}) ([]interface{}, error) {
	return u.broadcastTo(0, request, reply)
}

// Sends a UDP message to a specific device but anticipates replies from more than one device
// because it may fall back to the broadcast address if the device ID has no configured IP
// address.
func (u *uhppote) broadcastTo(serialNumber uint32, request, reply interface{}) ([]interface{}, error) {
	replies := []interface{}{}
	dest := u.broadcastAddress()

	if device, ok := u.devices[serialNumber]; ok {
		if device.Address != nil {
			dest = device.Address
		}
	}

	m, err := codec.Marshal(request)
	if err != nil {
		return nil, err
	}

	responses, err := u.driver.Broadcast(m, dest)
	if err != nil {
		return replies, err
	}

	for _, bytes := range responses {
		// ... discard invalid replies
		if len(bytes) != 64 {
			u.debugf(" ... receive error", fmt.Errorf("invalid message length - expected:%v, got:%v", 64, len(bytes)))
			continue
		}

		// ... discard replies without a valid device ID
		if deviceID := binary.LittleEndian.Uint32(bytes[4:8]); serialNumber != 0 && deviceID != serialNumber {
			u.debugf(" ... receive error", fmt.Errorf("invalid device ID - expected:%v, got:%v", serialNumber, deviceID))
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

func (u *uhppote) send(serialNumber uint32, request, reply interface{}) error {
	dest := u.broadcastAddress()
	if device, ok := u.devices[serialNumber]; ok {
		if device.Address != nil {
			dest = device.Address
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
	bind := u.listenAddress()
	if bind.Port == 0 {
		return fmt.Errorf("Listen requires a non-zero UDP port")
	}

	c, err := u.open(bind)
	if err != nil {
		return err
	}

	defer func() {
		c.Close()
	}()

	closed := false
	go func() {
		<-q
		closed = true
		c.Close()
	}()

	listener.OnConnected()

	m := make([]byte, 2048)

	for {
		u.debugf(" ... listening", nil)

		N, remote, err := c.ReadFromUDP(m)
		if err != nil {
			if closed {
				return nil
			}

			return fmt.Errorf("Failed to read from UDP socket [%v]", err)
		}

		u.debugf(fmt.Sprintf(" ... received %v bytes from %v\n ... response\n%s\n", N, remote, dump(m[:N], " ...          ")), nil)

		e := event{}
		if err := codec.Unmarshal(m[:N], &e); err != nil {
			if !listener.OnError(err) {
				return fmt.Errorf("FATAL ERROR: unable to unmarshal event [%v]", err)
			}

			continue
		}

		p <- &e
	}
}

func (u *uhppote) open(addr *net.UDPAddr) (*net.UDPConn, error) {
	connection, err := net.ListenUDP("udp", addr)
	if err != nil {
		return nil, fmt.Errorf("Failed to open UDP socket [%v]", err)
	}

	return connection, nil
}

func (u *uhppote) bindAddress() *net.UDPAddr {
	if u.bindAddr != nil {
		return u.bindAddr
	}

	addr := net.UDPAddr{
		IP:   make(net.IP, net.IPv4len),
		Port: 0,
		Zone: "",
	}

	copy(addr.IP, net.IPv4zero)

	return &addr
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

func (u *uhppote) listenAddress() *net.UDPAddr {
	if u.listenAddr != nil {
		return u.listenAddr
	}

	addr := net.UDPAddr{
		IP:   make(net.IP, net.IPv4len),
		Port: 60001,
		Zone: "",
	}

	copy(addr.IP, net.IPv4zero)

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

func dump(m []byte, prefix string) string {
	regex := regexp.MustCompile("(?m)^(.*)")

	return fmt.Sprintf("%s", regex.ReplaceAllString(hex.Dump(m), prefix+"$1"))
}
