using System;
using System.Collections.Generic;
using static System.Console;
using static System.String;

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
    const uint EVENT_INDEX = 51;
    const byte DOOR = 4;
    const byte PROFILE_ID = 49;

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
        new test("open-door", OpenDoor),
        new test("get-cards", GetCards),
        new test("get-card", GetCard),
        new test("get-card-by-index", GetCardByIndex),
        new test("put-card", PutCard),
        new test("delete-card", DeleteCard),
        new test("delete-cards", DeleteCards),
        new test("get-event-index", GetEventIndex),
        new test("set-event-index", SetEventIndex),
        new test("get-event", GetEvent),
        new test("record-special-events", RecordSpecialEvents),
        new test("get-time-profile", GetTimeProfile),
        new test("set-time-profile", SetTimeProfile),
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
                WriteLine();
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
            WriteLine();
            WriteLine(String.Format("   *** ERROR: invalid command ({0})", cmd));
            WriteLine();

            usage();
            Environment.Exit(-1);

        } catch (Exception e) {
            WriteLine(String.Format("  *** ERROR: {0}", e.Message));
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
        string tag = "get-devices";
        bool ok = true;

        uint[] devices = u.GetDevices();

        if (devices.Length != 3) {
            WriteLine(Format("get-devices: incorrect device count - expected:{0}, got:{1}", 3, devices.Length));
            ok = false;
        } else if (devices[0] != 201020304 || devices[1] != 303986753 || devices[2] != 405419896) {
            WriteLine(Format("get-devices: incorrect device list - expected:[ {0},{1},{2} ], got:[ {3},{4},{5} ]",
                             201020304, 303986753, 405419896,
                             devices[0], devices[1], devices[2]));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool GetDevice(Uhppoted u) {
        string tag = "get-device";
        bool ok = true;

        Device device = u.GetDevice(DEVICE_ID);

        if (device.ID != 405419896) {
            WriteLine(Format("get-device: incorrect device ID - expected:{0}, got:{1}", 405419896, device.ID));
            ok = false;
        }

        if (device.address != "192.168.1.101") {
            WriteLine(Format("get-device: incorrect IP address - expected:{0}, got:{1}", "192.168.1.101", device.address));
            ok = false;
        }

        if (device.subnet != "255.255.255.0") {
            WriteLine(Format("get-device: incorrect subnet mask - expected:{0}, got:{1}", "255.255.255.0", device.subnet));
            ok = false;
        }

        if (device.gateway != "192.168.1.1") {
            WriteLine(Format("get-device: incorrect gateway address - expected:{0}, got:{1}", "192.168.1.1", device.gateway));
            ok = false;
        }

        if (device.MAC != "00:12:23:34:45:56") {
            WriteLine(Format("get-device: incorrect MAC address - expected:{0}, got:{1}", "00:12:23:34:45:56", device.MAC));
            ok = false;
        }

        if (device.version != "v8.92") {
            WriteLine(Format("get-device: incorrect version - expected:{0}, got:{1}", "v8.92", device.version));
            ok = false;
        }

        if (device.date != "2018-11-05") {
            WriteLine(Format("get-device: incorrect date - expected:{0}, got:{1}", "2018-11-05", device.date));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool SetAddress(Uhppoted u) {
        string tag = "set-address";

        u.SetAddress(DEVICE_ID, "192.168.1.125", "255.255.254.0", "192.168.1.0");

        return passed(tag);
    }

    static bool GetStatus(Uhppoted u) {
        string tag = "get-status";
        bool ok = true;

        Status status = u.GetStatus(DEVICE_ID);

        if (status.ID != 405419896) {
            WriteLine(Format("get-status: incorrect device ID - expected:{0}, got:{1}", 405419896, status.ID));
            ok = false;
        }

        if (status.sysdatetime != "2022-03-19 15:48:32") {
            WriteLine(Format("get-status: incorrect system date/time - expected:{0}, got:{1}", "2022-03-19 15:48:32", status.sysdatetime));
            ok = false;
        }

        if (status.doors[0] != 1 || status.doors[1] != 0 || status.doors[2] != 0 || status.doors[3] != 1) {
            WriteLine(Format("get-status: incorrect doors state - expected:[{0},{1},{2},{3}], got:[{4},{5},{6},{7}]",
                             1, 0, 0, 1,
                             status.doors[0], status.doors[1], status.doors[2], status.doors[3]));
            ok = false;
        }

        if (status.buttons[0] != 1 || status.buttons[1] != 0 || status.buttons[2] != 1 || status.buttons[3] != 0) {
            WriteLine(Format("get-status: incorrect buttons state - expected:[{0},{1},{2},{3}], got:[{4},{5},{6},{7}]",
                             1, 0, 0, 1,
                             status.buttons[0], status.buttons[1], status.buttons[2], status.buttons[3]));
            ok = false;
        }

        if (status.relays != 0x12) {
            WriteLine(Format("get-status: incorrect relay state - expected:{0}, got:{1}", 0x12, status.relays));
            ok = false;
        }

        if (status.inputs != 0x34) {
            WriteLine(Format("get-status: incorrect inputs state - expected:{0}, got:{1}", 0x34, status.inputs));
            ok = false;
        }

        if (status.syserror != 0x56) {
            WriteLine(Format("get-status: incorrect system error - expected:{0}, got:{1}", 0x56, status.syserror));
            ok = false;
        }

        if (status.info != 253) {
            WriteLine(Format("get-status: incorrect special info - expected:{0}, got:{1}", 253, status.info));
            ok = false;
        }

        if (status.seqno != 9876) {
            WriteLine(Format("get-status: incorrect sequence number - expected:{0}, got:{1}", 9876, status.seqno));
            ok = false;
        }

        if (status.evt.timestamp != "2022-01-02 12:34:56") {
            WriteLine(Format("get-status: incorrect event timestamp - expected:{0}, got:{1}", "2022-01-02 12:34:56", status.evt.timestamp));
            ok = false;
        }

        if (status.evt.index != 135) {
            WriteLine(Format("get-status: incorrect event index - expected:{0}, got:{1}", 135, status.evt.index));
            ok = false;
        }

        if (status.evt.eventType != 6) {
            WriteLine(Format("get-status: incorrect event type - expected:{0}, got:{1}", 6, status.evt.eventType));
            ok = false;
        }

        if (!status.evt.granted) {
            WriteLine(Format("get-status: incorrect event granted - expected:{0}, got:{1}", 1, status.evt.granted));
            ok = false;
        }

        if (status.evt.door != 3) {
            WriteLine(Format("get-status: incorrect event door - expected:{0}, got:{1}", 3, status.evt.door));
            ok = false;
        }

        if (status.evt.direction != 1) {
            WriteLine(Format("get-status: incorrect event direction - expected:{0}, got:{1}", 1, status.evt.direction));
            ok = false;
        }

        if (status.evt.card != 8100023) {
            WriteLine(Format("get-status: incorrect event card - expected:{0}, got:{1}", 8100023, status.evt.card));
            ok = false;
        }

        if (status.evt.reason != 21) {
            WriteLine(Format("get-status: incorrect event reason - expected:{0}, got:{1}", 21, status.evt.reason));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool GetTime(Uhppoted u) {
        string tag = "get-time";
        bool ok = true;

        string datetime = u.GetTime(DEVICE_ID);

        if (datetime != "2022-01-02 12:34:56") {
            WriteLine(Format("get-time: incorrect date/time - expected:{0}, got:{1}", "2022-01-02 12:34:56", datetime));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool SetTime(Uhppoted u) {
        string tag = "set-time";

        u.SetTime(DEVICE_ID, "2022-03-23 12:24:17");

        return passed(tag);
    }

    static bool GetListener(Uhppoted u) {
        string tag = "get-listener";
        bool ok = true;

        string listener = u.GetListener(DEVICE_ID);

        if (listener != "192.168.1.100:60001") {
            WriteLine(Format("get-listener: incorrect event listener address - expected:{0}, got:{1}", "192.168.1.100:60001", listener));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool SetListener(Uhppoted u) {
        string tag = "set-listener";

        u.SetListener(DEVICE_ID, "192.168.1.100:60001");

        return passed(tag);
    }

    static bool GetDoorControl(Uhppoted u) {
        string tag = "get-door-control";
        bool ok = true;

        DoorControl control = u.GetDoorControl(DEVICE_ID, DOOR);

        if (control.mode != 3) {
            WriteLine(Format("get-door-control: incorrect door control mode - expected:{0}, got:{1}", 3, control.mode));
            ok = false;
        }

        if (control.delay != 7) {
            WriteLine(Format("get-door-control: incorrect door open delay - expected:{0}, got:{1}", 7, control.delay));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool SetDoorControl(Uhppoted u) {
        string tag = "set-door-control";

        u.SetDoorControl(DEVICE_ID, DOOR, ControlModes.NormallyClosed, 6);

        return passed(tag);
    }

    static bool OpenDoor(Uhppoted u) {
        string tag = "open-door";

        u.OpenDoor(DEVICE_ID, DOOR);

        return passed(tag);
    }

    static bool GetCards(Uhppoted u) {
        string tag = "get-cards";
        bool ok = true;

        int cards = u.GetCards(DEVICE_ID);

        if (cards != 39) {
            WriteLine(Format("get-cards: incorrect card count - expected:{0}, got:{1}", 39, cards));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool GetCard(Uhppoted u) {
        string tag = "get-card";
        bool ok = true;

        Card card = u.GetCard(DEVICE_ID, CARD_NUMBER);

        if (card.cardNumber != 8165538) {
            WriteLine(Format("get-card: incorrect card number - expected:{0}, got:{1}", 8165538, card.cardNumber));
            ok = false;
        }

        if (card.from != "2022-01-01") {
            WriteLine(Format("get-card: incorrect 'from' date - expected:{0}, got:{1}", "2022-01-01", card.from));
            ok = false;
        }

        if (card.to != "2022-12-31") {
            WriteLine(Format("get-card: incorrect 'to' date - expected:{0}, got:{1}", "2022-12-31", card.to));
            ok = false;
        }

        if (card.doors[0] != 0) {
            WriteLine(Format("get-card: incorrect doors[1] - expected:{0}, got:{1}", 0, card.doors[0]));
            ok = false;
        }

        if (card.doors[1] != 1) {
            WriteLine(Format("get-card: incorrect doors[2] - expected:{0}, got:{1}", 1, card.doors[1]));
            ok = false;
        }

        if (card.doors[2] != 31) {
            WriteLine(Format("get-card: incorrect doors[3] - expected:{0}, got:{1}", 31, card.doors[2]));
            ok = false;
        }

        if (card.doors[3] != 75) {
            WriteLine(Format("get-card: incorrect doors[4] - expected:{0}, got:{1}", 75, card.doors[3]));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool GetCardByIndex(Uhppoted u) {
        string tag = "get-card-by-index";
        bool ok = true;

        Card card = u.GetCardByIndex(DEVICE_ID, CARD_INDEX);

        if (card.cardNumber != 8165538) {
            WriteLine(Format("{0}: incorrect card number - expected:{1}, got:{2}", tag, 8165538, card.cardNumber));
            ok = false;
        }

        if (card.from != "2022-01-01") {
            WriteLine(Format("get-card: incorrect 'from' date - expected:{1}, got:{2}", tag, "2022-01-01", card.from));
            ok = false;
        }

        if (card.to != "2022-12-31") {
            WriteLine(Format("{0}: incorrect 'to' date - expected:{1}, got:{2}", tag, "2022-12-31", card.to));
            ok = false;
        }

        if (card.doors[0] != 0) {
            WriteLine(Format("{0}: incorrect doors[1] - expected:{1}, got:{2}", tag, 0, card.doors[0]));
            ok = false;
        }

        if (card.doors[1] != 1) {
            WriteLine(Format("{0}: incorrect doors[2] - expected:{1}, got:{2}", tag, 1, card.doors[1]));
            ok = false;
        }

        if (card.doors[2] != 31) {
            WriteLine(Format("{0}: incorrect doors[3] - expected:{1}, got:{2}", tag, 31, card.doors[2]));
            ok = false;
        }

        if (card.doors[3] != 75) {
            WriteLine(Format("{0}: incorrect doors[4] - expected:{1}, got:{2}", tag, 75, card.doors[3]));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool PutCard(Uhppoted u) {
        string tag = "put-card";
        byte[] doors = { 0, 1, 31, 75 };

        u.PutCard(DEVICE_ID, CARD_NUMBER, "2022-01-01", "2022-12-31", doors);

        return passed(tag);
    }

    static bool DeleteCard(Uhppoted u) {
        string tag = "delete-card";

        u.DeleteCard(DEVICE_ID, CARD_NUMBER);

        return passed(tag);
    }

    static bool DeleteCards(Uhppoted u) {
        string tag = "delete-cards";

        u.DeleteCards(DEVICE_ID);

        return passed(tag);
    }

    static bool GetEventIndex(Uhppoted u) {
        string tag = "get-event-index";
        uint index = u.GetEventIndex(DEVICE_ID);
        uint expected = 47;
        bool ok = true;

        if (index != expected) {
            WriteLine(Format("{0}: incorrect index - expected:{1}, got:{2}", tag, expected, index));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool SetEventIndex(Uhppoted u) {
        string tag = "set-event-index";

        u.SetEventIndex(DEVICE_ID, EVENT_INDEX);

        return passed(tag);
    }

    static bool GetEvent(Uhppoted u) {
        string tag = "get-event";
        bool ok = true;

        Event evt = u.GetEvent(DEVICE_ID, EVENT_INDEX);

        if (evt.index != 51) {
            WriteLine(Format("{0}: incorrect event index - expected:{1}, got:{2}", tag, 51, evt.index));
            ok = false;
        }

        if (evt.timestamp != "2022-04-15 12:29:15") {
            WriteLine(Format("{0}: incorrect event timestamp - expected:{1}, got:{2}", tag, "2022-04-15 12:29:15", evt.timestamp));
            ok = false;
        }

        if (evt.eventType != 6) {
            WriteLine(Format("{0}: incorrect event type - expected:{1}, got:{2}", tag, 6, evt.eventType));
            ok = false;
        }

        if (!evt.granted) {
            WriteLine(Format("{0}: incorrect event granted - expected:{1}, got:{2}", tag, true, evt.granted));
            ok = false;
        }

        if (evt.door != 3) {
            WriteLine(Format("{0}: incorrect event door - expected:{1}, got:{2}", tag, 3, evt.door));
            ok = false;
        }

        if (evt.direction != 1) {
            WriteLine(Format("{0}: incorrect event direction - expected:{1}, got:{2}", tag, 1, evt.direction));
            ok = false;
        }

        if (evt.card != 8165538) {
            WriteLine(Format("{0}: incorrect event card - expected:{1}, got:{2}", tag, 8165538, evt.card));
            ok = false;
        }

        if (evt.reason != 21) {
            WriteLine(Format("{0}: incorrect event reason - expected:{1}, got:{2}", tag, 21, evt.reason));
            ok = false;
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool RecordSpecialEvents(Uhppoted u) {
        string tag = "record-special-events";

        u.RecordSpecialEvents(DEVICE_ID, true);

        return passed(tag);
    }

    static bool GetTimeProfile(Uhppoted u) {
        string tag = "get-time-profile";
        TimeProfile profile = u.GetTimeProfile(DEVICE_ID, PROFILE_ID);
        bool ok = true;

        if (profile.ID != 49) {
            WriteLine(Format("{0}: incorrect profile ID - expected:{1}, got:{2}", tag, 49, profile.ID));
            ok = false;
        }

        if (profile.linked != 71) {
            WriteLine(Format("{0}: incorrect linked profile - expected:{1}, got:{2}", tag, 71, profile.linked));
            ok = false;
        }

        if (profile.from != "2022-02-01") {
            WriteLine(Format("{0}: incorrect profile 'from' date - expected:{1}, got:{2}", tag, "2022-02-01", profile.from));
            ok = false;
        }

        if (profile.to != "2022-06-30") {
            WriteLine(Format("{0}: incorrect profile 'to' date - expected:{1}, got:{2}", tag, "2022-06-30", profile.to));
            ok = false;
        }

        if (!profile.monday) {
            WriteLine(Format("{0}: incorrect profile 'monday' - expected:{1}, got:{2}", tag, true, profile.monday));
            ok = false;
        }

        if (profile.tuesday) {
            WriteLine(Format("{0}: incorrect profile 'tuesday' - expected:{1}, got:{2}", tag, false, profile.tuesday));
            ok = false;
        }

        if (!profile.wednesday) {
            WriteLine(Format("{0}: incorrect profile 'wednesday' - expected:{1}, got:{2}", tag, true, profile.wednesday));
            ok = false;
        }

        if (!profile.thursday) {
            WriteLine(Format("{0}: incorrect profile 'thursday' - expected:{1}, got:{2}", tag, true, profile.thursday));
            ok = false;
        }

        if (profile.friday) {
            WriteLine(Format("{0}: incorrect profile 'friday' - expected:{1}, got:{2}", tag, false, profile.friday));
            ok = false;
        }

        if (profile.saturday) {
            WriteLine(Format("{0}: incorrect profile 'saturday' - expected:{1}, got:{2}", tag, false, profile.saturday));
            ok = false;
        }

        if (!profile.sunday) {
            WriteLine(Format("{0}: incorrect profile 'sunday' - expected:{1}, got:{2}", tag, true, profile.sunday));
            ok = false;
        }

        if (profile.segment1start != "08:30") {
            WriteLine(Format("{0}: incorrect profile segment 1 start - expected:{1}, got:{2}", tag, "08:30", profile.segment1start));
            ok = false;
        }

        if (profile.segment1end != "11:30") {
            WriteLine(Format("{0}: incorrect profile segment 1 end - expected:{1}, got:{2}", tag, "11:30", profile.segment1end));
            ok = false;
        }

        if (profile.segment2start != "00:00") {
            WriteLine(Format("{0}: incorrect profile segment 2 start - expected:{1}, got:{2}", tag, "", profile.segment2start));
            ok = false;
        }

        if (profile.segment2end != "00:00") {
            WriteLine(Format("{0}: incorrect profile segment 2 end - expected:{1}, got:{2}", tag, "", profile.segment2end));
            ok = false;
        }

        if (profile.segment3start != "00:00") {
            WriteLine(Format("{0}: incorrect profile segment 3 start - expected:{1}, got:{2}", tag, "", profile.segment3start));
            ok = false;
        }

        if (profile.segment3end != "18:00") {
            WriteLine(Format("{0}: incorrect profile segment 3 end - expected:{1}, got:{2}", tag, "", profile.segment3end));
            ok = false;
        }
        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool SetTimeProfile(Uhppoted u) {
        string tag = "set-time-profile";
        TimeProfile profile = new TimeProfile(PROFILE_ID, 71, "2022-02-01", "2022-06-30",
                                              true, false, true, true, false, false, true,
                                              "08:30", "11:30",
                                              "", "",
                                              "", "18:00");

        u.SetTimeProfile(DEVICE_ID, profile);

        return passed(tag);
    }

    static bool passed(string tag) {
        WriteLine(String.Format("{0, -21} {1}", tag, "ok"));

        return true;
    }

    static bool failed(string tag) {
        WriteLine(String.Format("{0, -21} {1}", tag, "failed"));

        return false;
    }

    static void usage() {
        WriteLine("   Usage: test <command>");
        WriteLine();
        WriteLine("   Supported commands:");

        foreach (var t in tests) {
            WriteLine(Format("      {0}", t.command));
        }

        WriteLine();
        WriteLine("   Defaults to 'all'");
        WriteLine();
    }
}
