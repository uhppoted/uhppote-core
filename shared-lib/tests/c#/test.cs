using System;

using uhppoted;

public class test {
    const uint DEVICEID = 405419896;

    public static void Main(string[] args) {
        string cmd = "";

        if (args.Length > 0) {
            cmd = args[0];
        }

        try {
            Controller[] controllers = { new Controller(405419896, "192.168.1.100"),
                                         new Controller(303986753, "192.168.1.100") };

            Uhppoted u = new Uhppoted("192.168.1.100", "192.168.1.100:60000", "192.168.1.100:60001", 2500, controllers, true);
            bool ok = true;

            switch (cmd) {
            case "get-devices":
                ok = GetDevices(u);
                break;

            case "get-device":
                ok = GetDevice(u, DEVICEID);
                break;

            case "set-address":
                ok = SetAddress(u, DEVICEID, "192.168.1.125", "255.255.255.254", "192.168.1.5");
                break;

            case "get-status":
                ok = GetStatus(u, DEVICEID);
                break;

            case "get-time":
                ok = GetTime(u, DEVICEID);
                break;

            case "set-time":
                ok = SetTime(u, DEVICEID, "2022-03-23 12:24:17");
                break;

            case "get-listener":
                ok = GetListener(u, DEVICEID);
                break;

            case "":
            case "all":
                ok = GetDevices(u) ? ok : false;
                ok = GetDevice(u, DEVICEID) ? ok : false;
                ok = SetAddress(u, DEVICEID, "192.168.1.125", "255.255.255.254", "192.168.1.5") ? ok : false;
                ok = GetStatus(u, DEVICEID) ? ok : false;
                ok = GetTime(u, DEVICEID) ? ok : false;
                ok = SetTime(u, DEVICEID, "2022-03-23 12:24:17");
                ok = GetListener(u, DEVICEID) ? ok : false;
                break;

            default:
                Console.WriteLine();
                Console.WriteLine(String.Format("  *** ERROR: invalid command ({0})", cmd));
                Console.WriteLine();
                usage();
                break;
            }

            if (!ok) {
                Environment.Exit(-1);
            }
        } catch (Exception e) {
            Console.WriteLine(String.Format("  *** ERROR: {0}", e.Message));
            Environment.Exit(-1);
        }
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

        if (ok) {
            Console.WriteLine("get-devices   ok");
        }

        return ok;
    }

    static bool GetDevice(Uhppoted u, uint deviceID) {
        Device device = u.GetDevice(deviceID);
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

        if (ok) {
            Console.WriteLine("get-device    ok");
        }

        return ok;
    }

    static bool SetAddress(Uhppoted u, uint deviceID, string address, string subnet, string gateway) {
        u.SetAddress(deviceID, address, subnet, gateway);

        Console.WriteLine("set-address   ok");

        return true;
    }

    static bool GetStatus(Uhppoted u, uint deviceID) {
        Status status = u.GetStatus(deviceID);
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

        if (ok) {
            Console.WriteLine("get-status    ok");
        }

        return ok;
    }

    static bool GetTime(Uhppoted u, uint deviceID) {
        string datetime = u.GetTime(deviceID);
        bool ok = true;

        if (datetime != "2022-01-02 12:34:56") {
            Console.WriteLine("get-time: incorrect date/time - expected:{0}, got:{1}", "2022-01-02 12:34:56", datetime);
            ok = false;
        }

        if (ok) {
            Console.WriteLine("get-time      ok");
        }

        return ok;
    }

    static bool SetTime(Uhppoted u, uint deviceID, string datetime) {
        u.SetTime(deviceID, datetime);

        Console.WriteLine("set-time      ok");

        return true;
    }

    static bool GetListener(Uhppoted u, uint deviceID) {
        string listener = u.GetListener(deviceID);
        bool ok = true;

        if (listener != "192.168.1.100:60001") {
            Console.WriteLine("get-listener: incorrect event listener address - expected:{0}, got:{1}", "192.168.1.100:60001", listener);
            ok = false;
        }

        if (ok) {
            Console.WriteLine("get-listener  ok");
        }

        return ok;
    }

    static void usage() {
        Console.WriteLine("   Usage: test <command>");
        Console.WriteLine();
        Console.WriteLine("   Supported commands:");
        Console.WriteLine("      get-devices");
        Console.WriteLine("      get-device");
        Console.WriteLine("      set-address");
        Console.WriteLine("      get-status");
        Console.WriteLine("      get-time");
        Console.WriteLine("      set-time");
        Console.WriteLine();
    }
}
