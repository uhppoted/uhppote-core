using System;
using System.Collections.Generic;

using uhppoted;

public class test {
    public string command;
    public Func<Uhppoted, bool> fn;

    public test(string command, Func<Uhppoted, bool> fn) {
        this.command = command;
        this.fn = fn;
    }
};

public class Tests {
    const uint DEVICE_ID = 405419896;
    const uint CARD_NUMBER = 8165538;
    const uint CARD_INDEX = 19;
    const byte DOOR = 4;

    static Controller[] controllers = { new Controller(405419896, "192.168.1.100"),
                                        new Controller(303986753, "192.168.1.100") };

    static test[] tests = {
        new test("get-devices", GetDevices),
        new test("get-device", GetDevice),
        new test("set-address", SetAddress),
        new test("get-status", GetStatus),
        new test("get-time", GetTime),
        new test("set-time", SetTime),
        new test("get-listener", GetListener),
        new test("set-listener", SetListener),
        new test("get-door-control", GetDoorControl),
        new test("set-door-control", SetDoorControl),
        new test("get-cards", GetCards),
        new test("get-card", GetCard),
        new test("get-card-by-index", GetCardByIndex),
        new test("put-card", PutCard),
        new test("delete-card", DeleteCard),
        new test("delete-cards", DeleteCards),
        new test("get-event-index", GetEventIndex),
    };

    public static void Main(string[] args) {
        string cmd = "";
        if (args.Length > 0) {
            cmd = args[0];
        }

        try {
            Uhppoted u = new Uhppoted("192.168.1.100", "192.168.1.100:60000", "192.168.1.100:60001", 2500, controllers, true);

            // ... usage
            if (cmd == "help") {
                Console.WriteLine();
                usage();
                return;
            }

            // ... all/default
            if (cmd == "" || cmd == "all") {
                if (!All(u)) {
                    Environment.Exit(-1);
                }

                return;
            }

            // ... named test
            test t = Array.Find(tests, v => v.command == cmd);
            if (t != null) {
                if (!t.fn(u)) {
                    Environment.Exit(-1);
                }

                return;
            }

            // ... invalid command
            Console.WriteLine();
            Console.WriteLine(String.Format("   *** ERROR: invalid command ({0})", cmd));
            Console.WriteLine();

            usage();
            Environment.Exit(-1);

        } catch (Exception e) {
            Console.WriteLine(String.Format("  *** ERROR: {0}", e.Message));
            Environment.Exit(-1);
        }
    }

    static bool All(Uhppoted u) {
        bool ok = true;

        foreach (var t in tests) {
            ok = t.fn(u) ? ok : false;
        }

        return ok;
    }

    static bool GetDevices(Uhppoted u) {
        uint[] devices = u.GetDevices();
        bool ok = true;

        if (devices.Length != 3) {
            Console.WriteLine("get-devices: incorrect device count - expected:{0}, got:{1}", 3, devices.Length);
            ok = false;
        } else if (devices[0] != 201020304 || devices[1] != 303986753 || devices[2] != 405419896) {
            Console.WriteLine("get-devices: incorrect device list - expected:[ {0},{1},{2} ], got:[ {3},{4},{5} ]",
                              201020304, 303986753, 405419896,
                              devices[0], devices[1], devices[2]);
            ok = false;
        }

        return result("get-devices", ok);
    }

    static bool GetDevice(Uhppoted u) {
        Device device = u.GetDevice(DEVICE_ID);
        bool ok = true;

        if (device.ID != 405419896) {
            Console.WriteLine("get-device: incorrect device ID - expected:{0}, got:{1}", 405419896, device.ID);
            ok = false;
        }

        if (device.address != "192.168.1.101") {
            Console.WriteLine("get-device: incorrect IP address - expected:{0}, got:{1}", "192.168.1.101", device.address);
            ok = false;
        }

        if (device.subnet != "255.255.255.0") {
            Console.WriteLine("get-device: incorrect subnet mask - expected:{0}, got:{1}", "255.255.255.0", device.subnet);
            ok = false;
        }

        if (device.gateway != "192.168.1.1") {
            Console.WriteLine("get-device: incorrect gateway address - expected:", "192.168.1.1", device.gateway);
            ok = false;
        }

        if (device.MAC != "00:12:23:34:45:56") {
            Console.WriteLine("get-device: incorrect MAC address - expected:{0}, got:{1}", "00:12:23:34:45:56", device.MAC);
            ok = false;
        }

        if (device.version != "v8.92") {
            Console.WriteLine("get-device: incorrect version - expected:{0}, got:{1}", "v8.92", device.version);
            ok = false;
        }

        if (device.date != "2018-11-05") {
            Console.WriteLine("get-device: incorrect date - expected:{0}, got:{1}", "2018-11-05", device.date);
            ok = false;
        }

        return result("get-device", ok);
    }

    static bool SetAddress(Uhppoted u) {
        u.SetAddress(DEVICE_ID, "192.168.1.125", "255.255.254.0", "192.168.1.0");

        return result("set-address", true);
    }

    static bool GetStatus(Uhppoted u) {
        Status status = u.GetStatus(DEVICE_ID);
        bool ok = true;

        if (status.ID != 405419896) {
            Console.WriteLine("get-status: incorrect device ID - expected:{0}, got:{1}", 405419896, status.ID);
            ok = false;
        }

        if (status.sysdatetime != "2022-03-19 15:48:32") {
            Console.WriteLine("get-status: incorrect system date/time - expected:{0}, got:{1}", "2022-03-19 15:48:32", status.sysdatetime);
            ok = false;
        }

        if (status.doors[0] != 1 || status.doors[1] != 0 || status.doors[2] != 0 || status.doors[3] != 1) {
            Console.WriteLine("get-status: incorrect doors state - expected:[{0},{1},{2},{3}], got:[{4},{5},{6},{7}]",
                              1, 0, 0, 1,
                              status.doors[0], status.doors[1], status.doors[2], status.doors[3]);
            ok = false;
        }

        if (status.buttons[0] != 1 || status.buttons[1] != 0 || status.buttons[2] != 1 || status.buttons[3] != 0) {
            Console.WriteLine("get-status: incorrect buttons state - expected:[{0},{1},{2},{3}], got:[{4},{5},{6},{7}]",
                              1, 0, 0, 1,
                              status.buttons[0], status.buttons[1], status.buttons[2], status.buttons[3]);
            ok = false;
        }

        if (status.relays != 0x12) {
            Console.WriteLine("get-status: incorrect relay state - expected:{0}, got:{1}", 0x12, status.relays);
            ok = false;
        }

        if (status.inputs != 0x34) {
            Console.WriteLine("get-status: incorrect inputs state - expected:{0}, got:{1}", 0x34, status.inputs);
            ok = false;
        }

        if (status.syserror != 0x56) {
            Console.WriteLine("get-status: incorrect system error - expected:{0}, got:{1}", 0x56, status.syserror);
            ok = false;
        }

        if (status.info != 253) {
            Console.WriteLine("get-status: incorrect special info - expected:{0}, got:{1}", 253, status.info);
            ok = false;
        }

        if (status.seqno != 9876) {
            Console.WriteLine("get-status: incorrect sequence number - expected:{0}, got:{1}", 9876, status.seqno);
            ok = false;
        }

        if (status.evt.timestamp != "2022-01-02 12:34:56") {
            Console.WriteLine("get-status: incorrect event timestamp - expected:{0}, got:{1}", "2022-01-02 12:34:56", status.evt.timestamp);
            ok = false;
        }

        if (status.evt.index != 135) {
            Console.WriteLine("get-status: incorrect event index - expected:{0}, got:{1}", 135, status.evt.index);
            ok = false;
        }

        if (status.evt.eventType != 6) {
            Console.WriteLine("get-status: incorrect event type - expected:{0}, got:{1}", 6, status.evt.eventType);
            ok = false;
        }

        if (!status.evt.granted) {
            Console.WriteLine("get-status: incorrect event granted - expected:{0}, got:{1}", 1, status.evt.granted);
            ok = false;
        }

        if (status.evt.door != 3) {
            Console.WriteLine("get-status: incorrect event door - expected:{0}, got:{1}", 3, status.evt.door);
            ok = false;
        }

        if (status.evt.direction != 1) {
            Console.WriteLine("get-status: incorrect event direction - expected:{0}, got:{1}", 1, status.evt.direction);
            ok = false;
        }

        if (status.evt.card != 8100023) {
            Console.WriteLine("get-status: incorrect event card - expected:{0}, got:{1}", 8100023, status.evt.card);
            ok = false;
        }

        if (status.evt.reason != 21) {
            Console.WriteLine("get-status: incorrect event reason - expected:{0}, got:{1}", 21, status.evt.reason);
            ok = false;
        }

        return result("get-status", ok);
    }

    static bool GetTime(Uhppoted u) {
        string datetime = u.GetTime(DEVICE_ID);
        bool ok = true;

        if (datetime != "2022-01-02 12:34:56") {
            Console.WriteLine("get-time: incorrect date/time - expected:{0}, got:{1}", "2022-01-02 12:34:56", datetime);
            ok = false;
        }

        return result("get-time", ok);
    }

    static bool SetTime(Uhppoted u) {
        u.SetTime(DEVICE_ID, "2022-03-23 12:24:17");

        return result("set-time", true);
    }

    static bool GetListener(Uhppoted u) {
        string listener = u.GetListener(DEVICE_ID);
        bool ok = true;

        if (listener != "192.168.1.100:60001") {
            Console.WriteLine("get-listener: incorrect event listener address - expected:{0}, got:{1}", "192.168.1.100:60001", listener);
            ok = false;
        }

        return result("get-listener", ok);
    }

    static bool SetListener(Uhppoted u) {
        u.SetListener(DEVICE_ID, "192.168.1.100:60001");

        return result("set-listener", true);
    }

    static bool GetDoorControl(Uhppoted u) {
        DoorControl control = u.GetDoorControl(DEVICE_ID, DOOR);
        bool ok = true;

        if (control.mode != 3) {
            Console.WriteLine("get-door-control: incorrect door control mode - expected:{0}, got:{1}", 3, control.mode);
            ok = false;
        }

        if (control.delay != 7) {
            Console.WriteLine("get-door-control: incorrect door open delay - expected:{0}, got:{1}", 7, control.delay);
            ok = false;
        }

        return result("get-door-control", ok);
    }

    static bool SetDoorControl(Uhppoted u) {
        u.SetDoorControl(DEVICE_ID, DOOR, ControlModes.NormallyClosed, 6);

        return result("set-door-control", true);
    }

    static bool GetCards(Uhppoted u) {
        int cards = u.GetCards(DEVICE_ID);
        bool ok = true;

        if (cards != 39) {
            Console.WriteLine("get-cards: incorrect card count - expected:{0}, got:{1}", 39, cards);
            ok = false;
        }

        return result("get-cards", ok);
    }

    static bool GetCard(Uhppoted u) {
        Card card = u.GetCard(DEVICE_ID, CARD_NUMBER);
        bool ok = true;

        if (card.cardNumber != 8165538) {
            Console.WriteLine("get-card: incorrect card number - expected:{0}, got:{1}", 8165538, card.cardNumber);
            ok = false;
        }

        if (card.from != "2022-01-01") {
            Console.WriteLine("get-card: incorrect 'from' date - expected:{0}, got:{1}", "2022-01-01", card.from);
            ok = false;
        }

        if (card.to != "2022-12-31") {
            Console.WriteLine("get-card: incorrect 'to' date - expected:{0}, got:{1}", "2022-12-31", card.to);
            ok = false;
        }

        if (card.doors[0] != 0) {
            Console.WriteLine("get-card: incorrect doors[1] - expected:{0}, got:{1}", 0, card.doors[0]);
            ok = false;
        }

        if (card.doors[1] != 1) {
            Console.WriteLine("get-card: incorrect doors[2] - expected:{0}, got:{1}", 1, card.doors[1]);
            ok = false;
        }

        if (card.doors[2] != 31) {
            Console.WriteLine("get-card: incorrect doors[3] - expected:{0}, got:{1}", 31, card.doors[2]);
            ok = false;
        }

        if (card.doors[3] != 75) {
            Console.WriteLine("get-card: incorrect doors[4] - expected:{0}, got:{1}", 75, card.doors[3]);
            ok = false;
        }

        return result("get-card", ok);
    }

    static bool GetCardByIndex(Uhppoted u) {
        Card card = u.GetCardByIndex(DEVICE_ID, CARD_INDEX);
        bool ok = true;

        if (card.cardNumber != 8165538) {
            Console.WriteLine("get-card-by-index: incorrect card number - expected:{0}, got:{1}", 8165538, card.cardNumber);
            ok = false;
        }

        if (card.from != "2022-01-01") {
            Console.WriteLine("get-card: incorrect 'from' date - expected:{0}, got:{1}", "2022-01-01", card.from);
            ok = false;
        }

        if (card.to != "2022-12-31") {
            Console.WriteLine("get-card-by-index: incorrect 'to' date - expected:{0}, got:{1}", "2022-12-31", card.to);
            ok = false;
        }

        if (card.doors[0] != 0) {
            Console.WriteLine("get-card-by-index: incorrect doors[1] - expected:{0}, got:{1}", 0, card.doors[0]);
            ok = false;
        }

        if (card.doors[1] != 1) {
            Console.WriteLine("get-card-by-index: incorrect doors[2] - expected:{0}, got:{1}", 1, card.doors[1]);
            ok = false;
        }

        if (card.doors[2] != 31) {
            Console.WriteLine("get-card-by-index: incorrect doors[3] - expected:{0}, got:{1}", 31, card.doors[2]);
            ok = false;
        }

        if (card.doors[3] != 75) {
            Console.WriteLine("get-card-by-index: incorrect doors[4] - expected:{0}, got:{1}", 75, card.doors[3]);
            ok = false;
        }

        return result("get-card-by-index", ok);
    }

    static bool PutCard(Uhppoted u) {
        byte[] doors = { 0, 1, 31, 75 };

        u.PutCard(DEVICE_ID, CARD_NUMBER, "2022-01-01", "2022-12-31", doors);

        return result("put-card", true);
    }

    static bool DeleteCard(Uhppoted u) {
        u.DeleteCard(DEVICE_ID, CARD_NUMBER);

        return result("delete-card", true);
    }

    static bool DeleteCards(Uhppoted u) {
        u.DeleteCards(DEVICE_ID);

        return result("delete-cards", true);
    }

    static bool GetEventIndex(Uhppoted u) {
        string tag = "get-event-index";
        int index = u.GetEventIndex(DEVICE_ID);
        int expected = 47;
        bool ok = true;

        if (index != expected) {
            Console.WriteLine("{0}: incorrect index - expected:{1}, got:{2}", tag, expected, index);
            ok = false;
        }

        return result(tag, ok);
    }

    static bool result(string test, bool ok) {
        if (ok) {
            Console.WriteLine(String.Format("{0, -17}  {1}", test, "ok"));
        }

        return ok;
    }

    static void usage() {
        Console.WriteLine("   Usage: test <command>");
        Console.WriteLine();
        Console.WriteLine("   Supported commands:");

        foreach (var t in tests) {
            Console.WriteLine("      {0}", t.command);
        }

        Console.WriteLine();
        Console.WriteLine("   Defaults to 'all'");
        Console.WriteLine();
    }
}
