using System;

using uhppoted;

public class example {
    public static void Main(string[] args) {
        if (args.Length < 1) {
            usage();
            return;
        }

        string cmd = args[0];

        if (cmd == "help") {
            help();
            return;
        }

        try {
            Controller[] controllers = { new Controller(405419896, "192.168.1.100"),
                                         new Controller(303986753, "192.168.1.100") };

            Uhppoted u = new Uhppoted("192.168.1.100", "192.168.1.100:60000", "192.168.1.100:60001", 2, controllers, true);

            switch (cmd) {
            case "get-devices":
                GetDevices(u);
                break;

            case "get-device":
                GetDevice(u, 405419896);
                break;

            case "set-address":
                SetAddress(u, 405419896, "192.168.1.125", "255.255.255.254", "192.168.1.5");
                break;

            case "get-status":
                GetStatus(u, 405419896);
                break;

            case "all":
                GetDevices(u);
                GetDevice(u, 405419896);
                SetAddress(u, 405419896, "192.168.1.125", "255.255.255.254", "192.168.1.5");
                break;

            default:
                Console.WriteLine(String.Format("  *** ERROR: invalid command ({0})", cmd));
                break;
            }

        } catch (Exception e) {
            Console.WriteLine(String.Format("  *** ERROR: {0}", e.Message));
        }
    }

    static void usage() {
        Console.WriteLine();
        Console.WriteLine("  Usage: mono example.exe <command>");
        Console.WriteLine();
        Console.WriteLine("         mono example.exe help for a list of commands");
        Console.WriteLine();
    }

    static void help() {
        Console.WriteLine();
        Console.WriteLine("Usage: python example.py <command>");
        Console.WriteLine();
        Console.WriteLine("  commands");
        Console.WriteLine("    get-devices");
        Console.WriteLine("    get-device");
        Console.WriteLine("    set-address");
        Console.WriteLine("    get-status");
        Console.WriteLine("    help");
        Console.WriteLine();
        Console.WriteLine("  get-devices");
        Console.WriteLine("    Retrieves a list of UHPPOTE controller IDs findable on the local LAN.");
        Console.WriteLine();
        Console.WriteLine("  get-device");
        Console.WriteLine("    Retrieves the basic device information for a single UHPPOTE controller.");
        Console.WriteLine();
        Console.WriteLine("  set-address");
        Console.WriteLine("    Sets the controller IPv4 address, subnet mask and gateway address.");
        Console.WriteLine();
        Console.WriteLine("  get-status");
        Console.WriteLine("    Retrieves the current device status for a single UHPPOTE controller.");
        Console.WriteLine();
        Console.WriteLine("  help");
        Console.WriteLine("    Displays this information.");
        Console.WriteLine();
    }

    static void GetDevices(Uhppoted u) {
        uint[] list = u.GetDevices();

        Console.WriteLine(String.Format("get-devices ({0})", list.Length));
        for (int i = 0; i < list.Length; i++) {
            Console.WriteLine(String.Format("  {0}", list[i]));
        }
        Console.WriteLine();
    }

    static void GetDevice(Uhppoted u, uint deviceID) {
        Device device = u.GetDevice(deviceID);

        Console.WriteLine(String.Format("get-device"));
        Console.WriteLine(String.Format("  ID:       {0}", device.ID));
        Console.WriteLine(String.Format("  IP:       {0}  {1}  {2}", device.address, device.subnet, device.gateway));
        Console.WriteLine(String.Format("  MAC:      {0}", device.MAC));
        Console.WriteLine(String.Format("  version:  {0}", device.version));
        Console.WriteLine(String.Format("  released: {0}", device.date));
        Console.WriteLine();
    }

    static void SetAddress(Uhppoted u, uint deviceID, string address, string subnet, string gateway) {
        u.SetAddress(deviceID, address, subnet, gateway);

        Console.WriteLine(String.Format("set-address"));
        Console.WriteLine(String.Format("  ID:      {0}", deviceID));
        Console.WriteLine(String.Format("  address: {0}", address));
        Console.WriteLine(String.Format("  subnet:  {0}", subnet));
        Console.WriteLine(String.Format("  gateway: {0}", gateway));
        Console.WriteLine();
    }

    static void GetStatus(Uhppoted u, uint deviceID) {
        Status status = u.GetStatus(deviceID);

        Console.WriteLine(String.Format("get-status"));
        Console.WriteLine(String.Format("  ID:        {0}", status.ID));
        Console.WriteLine(String.Format("  timestamp: {0}", status.sysdatetime));
        Console.WriteLine(String.Format("  doors:     {0} {1} {2} {3}", status.doors[0], status.doors[1], status.doors[2], status.doors[3]));
        Console.WriteLine(String.Format("  buttons:   {0} {1} {2} {3}", status.buttons[0], status.buttons[1], status.buttons[2], status.buttons[3]));
        Console.WriteLine(String.Format("  relays:    {0:X}", status.relays));
        Console.WriteLine(String.Format("  inputs:    {0:X}", status.inputs));
        Console.WriteLine(String.Format("  syserror:  {0}", status.syserror));
        Console.WriteLine(String.Format("  info:      {0:X}", status.info));
        Console.WriteLine(String.Format("  seqno:     {0}", status.seqno));
        Console.WriteLine(String.Format("  event timestamp: {0}", status.evt.timestamp));
        Console.WriteLine(String.Format("        index:     {0}", status.evt.index));
        Console.WriteLine(String.Format("        type:      {0}", status.evt.eventType));
        Console.WriteLine(String.Format("        granted:   {0}", status.evt.granted));
        Console.WriteLine(String.Format("        door:      {0}", status.evt.door));
        Console.WriteLine(String.Format("        direction: {0}", status.evt.direction));
        Console.WriteLine(String.Format("        card:      {0}", status.evt.card));
        Console.WriteLine(String.Format("        reason:    {0}", status.evt.reason));

        Console.WriteLine();
    }
}