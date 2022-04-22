using System;
using System.Runtime.InteropServices;
using System.Text;

namespace uhppoted {

public class Controller {
    public uint ID;
    public string address;

    public Controller(uint ID, string address) {
        this.ID = ID;
        this.address = address;
    }
};

public class UhppotedException : Exception {
    public UhppotedException(string message) : base(message) {}
};

public class Device {
    public uint ID;
    public string address;
    public string subnet;
    public string gateway;
    public string MAC;
    public string version;
    public string date;

    public Device(uint ID, string address, string subnet, string gateway,
                  string MAC, string version, string date) {
        this.ID = ID;
        this.address = address;
        this.subnet = subnet;
        this.gateway = gateway;
        this.MAC = MAC;
        this.version = version;
        this.date = date;
    }
};

public class Event {
    public string timestamp;
    public uint index;
    public byte eventType;
    public bool granted;
    public byte door;
    public byte direction;
    public uint card;
    public byte reason;

    public Event(string timestamp, uint index, byte eventType, bool granted,
                 byte door, byte direction, uint card, byte reason) {
        this.timestamp = timestamp;
        this.index = index;
        this.eventType = eventType;
        this.granted = granted;
        this.door = door;
        this.direction = direction;
        this.card = card;
        this.reason = reason;
    }
};

public class Status {
    public uint ID;
    public string sysdatetime;
    public byte[] doors;
    public byte[] buttons;
    public byte relays;
    public byte inputs;
    public byte syserror;
    public byte info;
    public uint seqno;
    public Event evt;

    public Status(uint ID, string sysdatetime, byte[] doors, byte[] buttons,
                  byte relays, byte inputs, byte syserror, byte info, uint seqno,
                  Event evt) {
        this.ID = ID;
        this.sysdatetime = sysdatetime;
        this.doors = doors;
        this.buttons = buttons;
        this.relays = relays;
        this.inputs = inputs;
        this.syserror = syserror;
        this.info = info;
        this.seqno = seqno;
        this.evt = evt;
    }
};

public class DoorControl {
    public byte mode;
    public byte delay;

    public DoorControl(byte mode, byte delay) {
        this.mode = mode;
        this.delay = delay;
    }
};

public static class ControlModes {
    public const byte NormallyOpen = 1;
    public const byte NormallyClosed = 2;
    public const byte Controlled = 3;
};

public class Card {
    public uint cardNumber;
    public string from;
    public string to;
    public byte[] doors;

    public Card(uint cardNumber, string from, string to, byte[] doors) {
        this.cardNumber = cardNumber;
        this.from = from;
        this.to = to;
        this.doors = doors;
    }
};

public class TimeProfile {
    public byte ID;
    public byte linked;
    public string from;
    public string to;
    public bool monday;
    public bool tuesday;
    public bool wednesday;
    public bool thursday;
    public bool friday;
    public bool saturday;
    public bool sunday;
    public string segment1start;
    public string segment1end;
    public string segment2start;
    public string segment2end;
    public string segment3start;
    public string segment3end;

    public TimeProfile(byte ID, byte linked, string from, string to,
                       bool monday, bool tuesday, bool wednesday, bool thursday, bool friday, bool saturday, bool sunday,
                       string segment1start, string segment1end,
                       string segment2start, string segment2end,
                       string segment3start, string segment3end) {
        this.ID = ID;
        this.linked = linked;
        this.from = from;
        this.to = to;

        this.monday = monday;
        this.tuesday = tuesday;
        this.wednesday = wednesday;
        this.thursday = thursday;
        this.friday = friday;
        this.saturday = saturday;
        this.sunday = sunday;

        this.segment1start = segment1start;
        this.segment1end = segment1end;
        this.segment2start = segment2start;
        this.segment2end = segment2end;
        this.segment3start = segment3start;
        this.segment3end = segment3end;
    }
};

public class Uhppoted : IDisposable {
    private UHPPOTE u = new UHPPOTE();

    public Uhppoted() {}

    public Uhppoted(string bind, string broadcast, string listen, int timeout,
                    Controller[] controllers, bool debug) {
        this.u.bind = bind;
        this.u.broadcast = broadcast;
        this.u.listen = listen;
        this.u.timeout = timeout;
        this.u.devices = IntPtr.Zero;
        this.u.debug = debug;

        uint N = (uint)controllers.Length;
        udevice[] list = new udevice[N];

        for (int ix = 0; ix < controllers.Length; ix++) {
            Controller c = controllers[ix];

            list[ix].ID = c.ID;
            list[ix].address = c.address;
        }

        int sz = Marshal.SizeOf(typeof(udevice));
        IntPtr p = Marshal.AllocHGlobal((int)N * sz);

        for (int ix = 0; ix < list.Length; ix++) {
            udevice d = list[ix];
            IntPtr q = p + ix * sz;

            Marshal.StructureToPtr(d, q, false);
        }

        udevices devices = new udevices();
        devices.N = 2;
        devices.devices = p;

        IntPtr r = Marshal.AllocHGlobal(Marshal.SizeOf(devices));
        Marshal.StructureToPtr(devices, r, false);

        this.u.devices = r;
    }

    ~Uhppoted() { dispose(); }

    public void Dispose() {
        dispose();
        GC.SuppressFinalize(this);
    }

    private void dispose() {
        IntPtr p = this.u.devices;

        if (p != IntPtr.Zero) {
            udevices devices = (udevices)Marshal.PtrToStructure(p, typeof(udevices));
            IntPtr q = devices.devices;

            Marshal.FreeHGlobal(q);
            Marshal.FreeHGlobal(p);
        }
    }

    [DllImport("libuhppoted.so")]
    private static extern string GetDevices(ref UHPPOTE u, ref int N, uint[] list);

    [DllImport("libuhppoted.so")]
    private static extern string GetDevice(ref UHPPOTE u, ref GoDevice device, uint deviceID);

    [DllImport("libuhppoted.so")]
    private static extern string SetAddress(ref UHPPOTE u, uint deviceID, string address, string subnet, string gateway);

    [DllImport("libuhppoted.so")]
    private static extern string GetStatus(ref UHPPOTE u, ref GoStatus status, uint deviceID);

    [DllImport("libuhppoted.so")]
    private static extern string GetTime(ref UHPPOTE u, ref string datetime, uint deviceID);

    [DllImport("libuhppoted.so")]
    private static extern string SetTime(ref UHPPOTE u, uint deviceID, string datetime);

    [DllImport("libuhppoted.so")]
    private static extern string GetListener(ref UHPPOTE u, ref string listener, uint deviceID);

    [DllImport("libuhppoted.so")]
    private static extern string SetListener(ref UHPPOTE u, uint deviceID, string listener);

    [DllImport("libuhppoted.so")]
    private static extern string GetDoorControl(ref UHPPOTE u, ref GoDoorControl c, uint deviceID, byte door);

    [DllImport("libuhppoted.so")]
    private static extern string SetDoorControl(ref UHPPOTE u, uint deviceID, byte door, byte mode, byte delay);

    [DllImport("libuhppoted.so")]
    private static extern string OpenDoor(ref UHPPOTE u, uint deviceID, byte door);

    [DllImport("libuhppoted.so")]
    private static extern string GetCards(ref UHPPOTE u, ref int N, uint deviceID);

    [DllImport("libuhppoted.so")]
    private static extern string GetCard(ref UHPPOTE u, ref GoCard card, uint deviceID, uint cardNumber);

    [DllImport("libuhppoted.so")]
    private static extern string GetCardByIndex(ref UHPPOTE u, ref GoCard card, uint deviceID, uint index);

    [DllImport("libuhppoted.so")]
    private static extern string PutCard(ref UHPPOTE u, uint deviceID, uint cardNumber, string from, string to, byte[] doors);

    [DllImport("libuhppoted.so")]
    private static extern string DeleteCard(ref UHPPOTE u, uint deviceID, uint cardNumber);

    [DllImport("libuhppoted.so")]
    private static extern string DeleteCards(ref UHPPOTE u, uint deviceID);

    [DllImport("libuhppoted.so")]
    private static extern string GetEventIndex(ref UHPPOTE u, ref uint index, uint deviceID);

    [DllImport("libuhppoted.so")]
    private static extern string SetEventIndex(ref UHPPOTE u, uint deviceID, uint index);

    [DllImport("libuhppoted.so")]
    private static extern string GetEvent(ref UHPPOTE u, ref GoEvent evt, uint deviceID, uint index);

    [DllImport("libuhppoted.so")]
    private static extern string RecordSpecialEvents(ref UHPPOTE u, uint deviceID, bool enabled);

    [DllImport("libuhppoted.so")]
    private static extern string GetTimeProfile(ref UHPPOTE u, ref GoTimeProfile profile, uint deviceID, byte profileID);

    [DllImport("libuhppoted.so")]
    private static extern string SetTimeProfile(ref UHPPOTE u, uint deviceID, ref GoTimeProfile profile);

    public uint[] GetDevices() {
        int N = 0;
        int count = N;
        uint[] slice;

        do {
            N += 16;
            count = N;
            slice = new uint[N];

            string err = GetDevices(ref this.u, ref count, slice);
            if (err != null && err != "") {
                throw new UhppotedException(err);
            }
        } while (N < count);

        uint[] list = new uint[count];

        Array.Copy(slice, list, list.Length);

        return list;
    }

    public Device GetDevice(uint deviceID) {
        GoDevice device = new GoDevice();

        string err = GetDevice(ref this.u, ref device, deviceID);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }

        return new Device(device.ID, device.address, device.subnet, device.gateway,
                          device.MAC, device.version, device.date);
    }

    public void SetAddress(uint deviceID, string address, string subnet,
                           string gateway) {
        string err = SetAddress(ref this.u, deviceID, address, subnet, gateway);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    public Status GetStatus(uint deviceID) {
        GoStatus status = new GoStatus();

        status.doors = Marshal.AllocHGlobal(4);
        status.buttons = Marshal.AllocHGlobal(4);
        status.evt = Marshal.AllocHGlobal(Marshal.SizeOf(typeof(GoEvent)));

        string err = GetStatus(ref this.u, ref status, deviceID);
        if (err != null && err != "") {
            Marshal.FreeHGlobal(status.doors);
            Marshal.FreeHGlobal(status.buttons);
            Marshal.FreeHGlobal(status.evt);

            throw new UhppotedException(err);
        }

        byte[] doors = new byte[4];
        byte[] buttons = new byte[4];
        GoEvent evt = (GoEvent)Marshal.PtrToStructure(status.evt, typeof(GoEvent));

        Marshal.Copy(status.doors, doors, 0, 4);
        Marshal.Copy(status.buttons, buttons, 0, 4);

        Event e = new Event(evt.timestamp,
                            evt.index,
                            evt.eventType,
                            evt.granted != 0,
                            evt.door,
                            evt.direction,
                            evt.card,
                            evt.reason);

        Marshal.FreeHGlobal(status.doors);
        Marshal.FreeHGlobal(status.buttons);
        Marshal.FreeHGlobal(status.evt);

        return new Status(status.ID,
                          status.sysdatetime,
                          doors,
                          buttons,
                          status.relays,
                          status.inputs,
                          status.syserror,
                          status.info,
                          status.seqno,
                          e);
    }

    public string GetTime(uint deviceID) {
        string datetime = "";

        string err = GetTime(ref this.u, ref datetime, deviceID);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }

        return datetime;
    }

    public void SetTime(uint deviceID, string datetime) {
        string err = SetTime(ref this.u, deviceID, datetime);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    public string GetListener(uint deviceID) {
        string listener = "";

        string err = GetListener(ref this.u, ref listener, deviceID);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }

        return listener;
    }

    public void SetListener(uint deviceID, string listener) {
        string err = SetListener(ref this.u, deviceID, listener);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    public DoorControl GetDoorControl(uint deviceID, byte door) {
        GoDoorControl control = new GoDoorControl();

        string err = GetDoorControl(ref this.u, ref control, deviceID, door);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }

        return new DoorControl(control.control, control.delay);
    }

    public void SetDoorControl(uint deviceID, byte door, byte mode, byte delay) {
        string err = SetDoorControl(ref this.u, deviceID, door, mode, delay);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    public void OpenDoor(uint deviceID, byte door) {
        string err = OpenDoor(ref this.u, deviceID, door);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    public int GetCards(uint deviceID) {
        int N = 0;

        string err = GetCards(ref this.u, ref N, deviceID);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }

        return N;
    }

    public Card GetCard(uint deviceID, uint cardNumber) {
        GoCard card = new GoCard();

        card.doors = Marshal.AllocHGlobal(4);

        string err = GetCard(ref this.u, ref card, deviceID, cardNumber);
        if (err != null && err != "") {
            Marshal.FreeHGlobal(card.doors);

            throw new UhppotedException(err);
        }

        byte[] doors = new byte[4];

        Marshal.Copy(card.doors, doors, 0, 4);
        Marshal.FreeHGlobal(card.doors);

        return new Card(card.cardNumber, card.from, card.to, doors);
    }

    public Card GetCardByIndex(uint deviceID, uint index) {
        GoCard card = new GoCard();

        card.doors = Marshal.AllocHGlobal(4);

        string err = GetCardByIndex(ref this.u, ref card, deviceID, index);
        if (err != null && err != "") {
            Marshal.FreeHGlobal(card.doors);

            throw new UhppotedException(err);
        }

        byte[] doors = new byte[4];

        Marshal.Copy(card.doors, doors, 0, 4);
        Marshal.FreeHGlobal(card.doors);

        return new Card(card.cardNumber, card.from, card.to, doors);
    }

    public void PutCard(uint deviceID, uint cardNumber, string from, string to, byte[] doors) {
        string err = PutCard(ref this.u, deviceID, cardNumber, from, to, doors);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    public void DeleteCard(uint deviceID, uint cardNumber) {
        string err = DeleteCard(ref this.u, deviceID, cardNumber);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    public void DeleteCards(uint deviceID) {
        string err = DeleteCards(ref this.u, deviceID);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    public uint GetEventIndex(uint deviceID) {
        uint index = 0;

        string err = GetEventIndex(ref this.u, ref index, deviceID);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }

        return index;
    }

    public void SetEventIndex(uint deviceID, uint index) {
        string err = SetEventIndex(ref this.u, deviceID, index);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    public Event GetEvent(uint deviceID, uint index) {
        GoEvent evt = new GoEvent();

        string err = GetEvent(ref this.u, ref evt, deviceID, index);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }

        return new Event(evt.timestamp,
                         evt.index,
                         evt.eventType,
                         evt.granted == 1,
                         evt.door,
                         evt.direction,
                         evt.card,
                         evt.reason);
    }

    public void RecordSpecialEvents(uint deviceID, bool enabled) {
        string err = RecordSpecialEvents(ref this.u, deviceID, enabled);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    public TimeProfile GetTimeProfile(uint deviceID, byte profileID) {
        GoTimeProfile profile = new GoTimeProfile();

        string err = GetTimeProfile(ref this.u, ref profile, deviceID, profileID);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }

        return new TimeProfile(profile.ID,
                               profile.linked,
                               profile.from,
                               profile.to,
                               profile.monday != 0,
                               profile.tuesday != 0,
                               profile.wednesday != 0,
                               profile.thursday != 0,
                               profile.friday != 0,
                               profile.saturday != 0,
                               profile.sunday != 0,
                               profile.segment1start, profile.segment1end,
                               profile.segment2start, profile.segment2end,
                               profile.segment3start, profile.segment3end);
    }

    public void SetTimeProfile(uint deviceID, TimeProfile p) {
        GoTimeProfile profile = new GoTimeProfile();

        profile.ID = p.ID;
        profile.linked = p.linked;
        profile.from = p.from;
        profile.to = p.to;
        profile.monday = p.monday ? (byte)1 : (byte)0;
        profile.tuesday = p.tuesday ? (byte)1 : (byte)0;
        profile.wednesday = p.wednesday ? (byte)1 : (byte)0;
        profile.thursday = p.thursday ? (byte)1 : (byte)0;
        profile.friday = p.friday ? (byte)1 : (byte)0;
        profile.saturday = p.saturday ? (byte)1 : (byte)0;
        profile.sunday = p.sunday ? (byte)1 : (byte)0;
        profile.segment1start = p.segment1start;
        profile.segment1end = p.segment1end;
        profile.segment2start = p.segment2start;
        profile.segment2end = p.segment2end;
        profile.segment3start = p.segment3start;
        profile.segment3end = p.segment3end;

        string err = SetTimeProfile(ref this.u, deviceID, ref profile);
        if (err != null && err != "") {
            throw new UhppotedException(err);
        }
    }

    // INTERNAL structs for DLL

    struct udevice {
        public uint ID;
        public string address;
    };

    struct udevices {
        public uint N;
        public IntPtr devices; // array of udevice *
    };

    struct UHPPOTE {
        public string bind;
        public string broadcast;
        public string listen;
        public int timeout;    // seconds, defaults to 5 if <= 0
        public IntPtr devices; // udevices * (optional list of non-local controller
                               // ID + address pairs)
        public bool debug;
    };

    struct GoDevice {
        public uint ID;
        public string address;
        public string subnet;
        public string gateway;
        public string MAC;
        public string version;
        public string date;
    };

    struct GoEvent {
        public string timestamp;
        public uint index;
        public byte eventType;
        public byte granted;
        public byte door;
        public byte direction;
        public uint card;
        public byte reason;
    };

    struct GoStatus {
        public uint ID;
        public string sysdatetime;
        public IntPtr doors;
        public IntPtr buttons;
        public byte relays;
        public byte inputs;
        public byte syserror;
        public byte info;
        public uint seqno;
        public IntPtr evt;
    };

    struct GoDoorControl {
        public byte control;
        public byte delay;
    };

    struct GoCard {
        public uint cardNumber;
        public string from;
        public string to;
        public IntPtr doors;
    };

    struct GoTimeProfile {
        public byte ID;
        public byte linked;
        public string from;
        public string to;
        public byte monday;
        public byte tuesday;
        public byte wednesday;
        public byte thursday;
        public byte friday;
        public byte saturday;
        public byte sunday;
        public string segment1start;
        public string segment1end;
        public string segment2start;
        public string segment2end;
        public string segment3start;
        public string segment3end;
    };
};
}
