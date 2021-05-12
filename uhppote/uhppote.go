package uhppote

import (
	"encoding/binary"
	"encoding/hex"
	"fmt"
	"net"
	"os"
	"regexp"
	"sync"
	"time"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
)

var VERSION string = "v0.7.x"
var guard sync.Mutex

type iuhppote interface {
	Send(serialNumber uint32, request, reply interface{}) error
	Broadcast(request, reply interface{}) ([]interface{}, error)
	BroadcastTo(serialNumber uint32, request, reply interface{}) ([]interface{}, error)

	BroadcastAddr() *net.UDPAddr
	DeviceList() map[uint32]*Device
}

type UHPPOTE struct {
	bindAddr      *net.UDPAddr
	broadcastAddr *net.UDPAddr
	listenAddr    *net.UDPAddr
	devices       map[uint32]*Device
	debug         bool
	driver        iuhppote
}

func NewUHPPOTE(bind, broadcast, listen net.UDPAddr, devices []Device, debug bool) UHPPOTE {
	uhppote := UHPPOTE{
		bindAddr:      &bind,
		broadcastAddr: &broadcast,
		listenAddr:    &listen,
		devices:       map[uint32]*Device{},
		debug:         debug,
	}

	uhppote.driver = &uhppote

	for _, device := range devices {
		uhppote.devices[device.DeviceID] = &device
	}

	return uhppote
}

func (u *UHPPOTE) DeviceList() map[uint32]*Device {
	if u != nil {
		return u.devices
	}

	return map[uint32]*Device{}
}

func (u *UHPPOTE) BroadcastAddr() *net.UDPAddr {
	if u != nil {
		return u.broadcastAddr
	}

	return nil
}

func (u *UHPPOTE) ListenAddr() *net.UDPAddr {
	if u != nil {
		return u.listenAddr
	}

	return nil
}

func (u *UHPPOTE) Send(serialNumber uint32, request, reply interface{}) error {
	bind := u.bindAddress()
	dest := u.broadcastAddress()

	if device, ok := u.devices[serialNumber]; ok {
		if device.Address != nil {
			dest = device.Address
		}
	}

	if bind.Port != 0 {
		guard.Lock()
		defer guard.Unlock()
	}

	c, err := u.open(bind)
	if err != nil {
		return err
	}

	defer c.Close()

	if err := u.send(c, dest, request); err != nil {
		return err
	} else if reply == nil {
		return nil
	}

	received := make(chan error)
	timer := time.NewTimer(5 * time.Second)
	defer timer.Stop()

	go func() {
		received <- u.receive(c, serialNumber, reply)
	}()

	select {
	case err := <-received:
		if err != nil {
			u.debugf(" ... receive error", err)
		}
		return err

	case <-timer.C:
		return fmt.Errorf("Timeout waiting for reply from %v", serialNumber)
	}
}

func (u *UHPPOTE) Broadcast(request, reply interface{}) ([]interface{}, error) {
	return u.BroadcastTo(0, request, reply)
}

// Sends a UDP message to a specific device but anticipates replies from more than one device
// because it may fall back to the broadcast address if the device ID has no configured IP
// address.
func (u *UHPPOTE) BroadcastTo(serialNumber uint32, request, reply interface{}) ([]interface{}, error) {
	replies := []interface{}{}
	dest := u.broadcastAddress()

	if device, ok := u.devices[serialNumber]; ok {
		if device.Address != nil {
			dest = device.Address
		}
	}

	m, err := u.broadcast(request, dest)
	if err != nil {
		return replies, err
	}

	for _, bytes := range m {
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

func (u *UHPPOTE) open(addr *net.UDPAddr) (*net.UDPConn, error) {
	connection, err := net.ListenUDP("udp", addr)
	if err != nil {
		return nil, fmt.Errorf("Failed to open UDP socket [%v]", err)
	}

	return connection, nil
}

func (u *UHPPOTE) send(connection *net.UDPConn, addr *net.UDPAddr, request interface{}) error {
	m, err := codec.Marshal(request)
	if err != nil {
		return err
	}

	u.debugf(fmt.Sprintf(" ... request\n%s\n", dump(m, " ...          ")), nil)

	N, err := connection.WriteTo(m, addr)

	if err != nil {
		return fmt.Errorf("Failed to write to UDP socket [%v]", err)
	}

	u.debugf(fmt.Sprintf(" ... sent %v bytes to %v\n", N, addr), nil)

	return nil
}

func (u *UHPPOTE) broadcast(request interface{}, addr *net.UDPAddr) ([][]byte, error) {
	m, err := codec.Marshal(request)
	if err != nil {
		return nil, err
	}

	u.debugf(fmt.Sprintf(" ... request\n%s\n", dump(m, " ...          ")), nil)

	bind := u.bindAddress()

	if bind.Port != 0 {
		guard.Lock()
		defer guard.Unlock()
	}

	connection, err := net.ListenUDP("udp", bind)
	if err != nil {
		return nil, fmt.Errorf("Failed to open UDP socket [%v]", err)
	}

	defer connection.Close()

	err = connection.SetWriteDeadline(time.Now().Add(5000 * time.Millisecond))
	if err != nil {
		return nil, fmt.Errorf("Failed to set UDP write timeout [%v]", err)
	}

	N, err := connection.WriteTo(m, addr)
	if err != nil {
		return nil, fmt.Errorf("Failed to write to UDP socket [%v]", err)
	}

	u.debugf(fmt.Sprintf(" ... sent %v bytes to %v\n", N, addr), nil)

	replies := make([][]byte, 0)
	go func() {
		for {
			reply := make([]byte, 2048)
			N, remote, err := connection.ReadFromUDP(reply)

			if err != nil {
				break
			} else {
				replies = append(replies, reply[:N])

				u.debugf(fmt.Sprintf(" ... received %v bytes from %v\n%s", N, remote, dump(reply[:N], " ...          ")), nil)
			}
		}
	}()

	time.Sleep(2500 * time.Millisecond)

	return replies, err
}

func (u *UHPPOTE) receive(c *net.UDPConn, serialNumber uint32, reply interface{}) error {
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

		// ... discard invalid replies
		if len(bytes) != 64 {
			u.debugf(" ... receive error", fmt.Errorf("invalid message length - expected:%v, got:%v", 64, len(bytes)))
			continue
		}

		// ... discard replies without a valid device ID
		if deviceID := binary.LittleEndian.Uint32(bytes[4:8]); deviceID != serialNumber {
			u.debugf(" ... receive error", fmt.Errorf("invalid device ID - expected:%v, got:%v", serialNumber, deviceID))
			continue
		}

		// .. discard unparseable messages
		if err := codec.Unmarshal(bytes, reply); err != nil {
			u.debugf(" ... receive error", err)
			continue
		}

		return nil
	}
}

func (u *UHPPOTE) listen(p chan *event, q chan os.Signal, listener Listener) error {
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

func (u *UHPPOTE) bindAddress() *net.UDPAddr {
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

func (u *UHPPOTE) broadcastAddress() *net.UDPAddr {
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

func (u *UHPPOTE) listenAddress() *net.UDPAddr {
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

func (u *UHPPOTE) debugf(msg string, err error) {
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
