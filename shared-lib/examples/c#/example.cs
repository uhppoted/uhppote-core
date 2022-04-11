using System;

using uhppoted;

public class command {
    public string cmd;
    public string help;
    public Action<Uhppoted, string[]> fn;

    public command(string cmd, string help) {
        this.cmd = cmd;
        this.help = help;
    }

    public command(string cmd, string help, Action<Uhppoted, string[]> fn) {
        this.cmd = cmd;
        this.help = help;
        this.fn = fn;
    }
};

public class example {
    const uint DEVICEID = 405419896;
    const uint CARD_NUMBER = 8000001;
    const uint CARD_INDEX = 7;

    static command[] commands = {
        new command("get-devices",
                    "Retrieves a list of UHPPOTE controller IDs findable on the local LAN.",
                    GetDevices),
        new command("get-device",
                    "Retrieves the basic device information for a single UHPPOTE controller."),
        new command("set-address",
                    "Sets the controller IPv4 address, subnet mask and gateway address."),
        new command("get-status",
                    "Retrieves a controller status."),
        new command("get-time",
                    "Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss)."),
        new command("set-time",
                    "Sets a controller current date/time (YYYY-MM-DD HH:mm:ss)."),
        new command("get-listener",
                    "Retrieves a controller's configured event listener address."),
        new command("set-listener",
                    "Configures a controller's event listener address and port."),
        new command("get-door-control",
                    "Retrieves the control state and open delay for a controller door."),
        new command("set-door-control",
                    "Sets the control mode and delay for a controller door."),
        new command("get-cards",
                    "Retrieves the number of cards stored on a controller."),
        new command("get-card",
                    "Retrieves the card detail for card number from a controller."),
        new command("get-card-by-index",
                    "Retrieves the card detail for the card stored at an index on a controller.",
                    GetCardByIndex),
        new command("put-card",
                    "Adds or updates the card detail for card number stored on a controller.",
                    PutCard),
        new command("delete-card",
                    "Deletes a card from a controller.",
                    DeleteCard),
    };

    public static void Main(string[] args) {
        if (args.Length < 1) {
            usage();
            return;
        }

        string cmd = args[0];

        try {
            Controller[] controllers = { new Controller(405419896, "192.168.1.100"),
                                         new Controller(303986753, "192.168.1.100") };

            Uhppoted u = new Uhppoted("192.168.1.100", "192.168.1.100:60000", "192.168.1.100:60001", 2500, controllers, true);

            switch (cmd) {
            case "help":
                help();
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

            case "get-time":
                GetTime(u, 405419896);
                break;

            case "set-time":
                SetTime(u, 405419896, DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss"));
                break;

            case "get-listener":
                GetListener(u, 405419896);
                break;

            case "set-listener":
                SetListener(u, 405419896, "192.168.1.100:60001");
                break;

            case "get-door-control":
                GetDoorControl(u, 405419896, 4);
                break;

            case "set-door-control":
                SetDoorControl(u, 405419896, 4, ControlModes.NormallyOpen, 9);
                break;

            case "get-cards":
                GetCards(u, 405419896);
                break;

            case "get-card":
                GetCard(u, 405419896, 8000001);
                break;

            default:
                foreach (command c in commands) {
                    if (c.cmd == cmd) {
                        c.fn(u, args);
                        return;
                    }
                }

                Console.WriteLine();
                Console.WriteLine(String.Format("  *** ERROR: invalid command ({0})", cmd));
                usage();
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
        Console.WriteLine("  Supported commands");

        foreach (command c in commands) {
            Console.WriteLine("    {0}", c.cmd);
        }

        Console.WriteLine();
    }

    static void help() {
        Console.WriteLine();
        Console.WriteLine("  Usage: mono example.exe <command>");
        Console.WriteLine();
        Console.WriteLine("  Commands");

        foreach (command c in commands) {
            Console.WriteLine("    {0,-17}  {1}", c.cmd, c.help);
        }

        Console.WriteLine();
    }

    static void GetDevices(Uhppoted u, string[] args) {
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

    static void GetTime(Uhppoted u, uint deviceID) {
        string datetime = u.GetTime(deviceID);

        Console.WriteLine(String.Format("get-time"));
        Console.WriteLine(String.Format("  date/time: {0}", datetime));
        Console.WriteLine();
    }

    static void SetTime(Uhppoted u, uint deviceID, string datetime) {
        u.SetTime(deviceID, datetime);

        Console.WriteLine(String.Format("set-time"));
        Console.WriteLine(String.Format("  ID:        {0}", deviceID));
        Console.WriteLine(String.Format("  date/time: {0}", datetime));
        Console.WriteLine();
    }

    static void GetListener(Uhppoted u, uint deviceID) {
        string listener = u.GetListener(deviceID);

        Console.WriteLine(String.Format("get-listener"));
        Console.WriteLine(String.Format("  listener: {0}", listener));
        Console.WriteLine();
    }

    static void SetListener(Uhppoted u, uint deviceID, string listener) {
        u.SetListener(deviceID, listener);

        Console.WriteLine(String.Format("set-listener"));
        Console.WriteLine(String.Format("  ID:             {0}", deviceID));
        Console.WriteLine(String.Format("  event listener: {0}", listener));
        Console.WriteLine();
    }

    static void GetDoorControl(Uhppoted u, uint deviceID, byte door) {
        DoorControl control = u.GetDoorControl(deviceID, door);

        Console.WriteLine(String.Format("get-door-control"));
        Console.WriteLine(String.Format("  ID:      {0}", deviceID));
        Console.WriteLine(String.Format("  door:    {0}", door));

        switch (control.mode) {
        case ControlModes.NormallyOpen:
            Console.WriteLine(String.Format("  mode:    {0}", "normally open"));
            break;

        case ControlModes.NormallyClosed:
            Console.WriteLine(String.Format("  mode:    {0}", "normally closed"));
            break;

        case ControlModes.Controlled:
            Console.WriteLine(String.Format("  mode:    {0}", "controlled"));
            break;

        default:
            Console.WriteLine(String.Format("  mode:    {0}", "???"));
            break;
        }

        Console.WriteLine(String.Format("  delay:   {0}", control.delay));
        Console.WriteLine();
    }

    static void SetDoorControl(Uhppoted u, uint deviceID, byte door, byte mode, byte delay) {
        u.SetDoorControl(deviceID, door, mode, delay);

        Console.WriteLine(String.Format("set-door-control"));
        Console.WriteLine(String.Format("  ID:      {0}", deviceID));
        Console.WriteLine(String.Format("  door:    {0}", door));

        switch (mode) {
        case ControlModes.NormallyOpen:
            Console.WriteLine(String.Format("  mode:    {0}", "normally open"));
            break;

        case ControlModes.NormallyClosed:
            Console.WriteLine(String.Format("  mode:    {0}", "normally closed"));
            break;

        case ControlModes.Controlled:
            Console.WriteLine(String.Format("  mode:    {0}", "controlled"));
            break;

        default:
            Console.WriteLine(String.Format("  mode:    {0}", "???"));
            break;
        }

        Console.WriteLine(String.Format("  delay:   {0}", delay));
        Console.WriteLine();
    }

    static void GetCards(Uhppoted u, uint deviceID) {
        int cards = u.GetCards(deviceID);

        Console.WriteLine(String.Format("get-cards"));
        Console.WriteLine(String.Format("  ID:    {0}", deviceID));
        Console.WriteLine(String.Format("  cards: {0}", cards));
        Console.WriteLine();
    }

    static void GetCard(Uhppoted u, uint deviceID, uint cardNumber) {
        Card card = u.GetCard(deviceID, cardNumber);

        Console.WriteLine(String.Format("get-card"));
        Console.WriteLine(String.Format("  ID:           {0}", deviceID));
        Console.WriteLine(String.Format("  card number:  {0}", card.cardNumber));
        Console.WriteLine(String.Format("       from:    {0}", card.from));
        Console.WriteLine(String.Format("       to:      {0}", card.to));
        Console.WriteLine(String.Format("       door[1]: {0}", card.doors[0]));
        Console.WriteLine(String.Format("       door[2]: {0}", card.doors[1]));
        Console.WriteLine(String.Format("       door[3]: {0}", card.doors[2]));
        Console.WriteLine(String.Format("       door[4]: {0}", card.doors[3]));
        Console.WriteLine();
    }

    static void GetCardByIndex(Uhppoted u, string[] args) {
        uint deviceID = DEVICEID;
        uint index = CARD_INDEX;

        Card card = u.GetCardByIndex(deviceID, index);

        Console.WriteLine(String.Format("get-card-by-index"));
        Console.WriteLine(String.Format("  ID:           {0}", deviceID));
        Console.WriteLine(String.Format("  index:        {0}", index));
        Console.WriteLine(String.Format("  card number:  {0}", card.cardNumber));
        Console.WriteLine(String.Format("       from:    {0}", card.from));
        Console.WriteLine(String.Format("       to:      {0}", card.to));
        Console.WriteLine(String.Format("       door[1]: {0}", card.doors[0]));
        Console.WriteLine(String.Format("       door[2]: {0}", card.doors[1]));
        Console.WriteLine(String.Format("       door[3]: {0}", card.doors[2]));
        Console.WriteLine(String.Format("       door[4]: {0}", card.doors[3]));
        Console.WriteLine();
    }

    static void PutCard(Uhppoted u, string[] args) {
        uint deviceID = DEVICEID;
        uint cardNumber = CARD_NUMBER;
        string from = "2022-01-01";
        string to = "2022-12-31";
        byte[] doors = { 0, 1, 31, 75 };

        u.PutCard(deviceID, cardNumber, from, to, doors);

        Console.WriteLine(String.Format("put-card"));
        Console.WriteLine(String.Format("  ID:           {0}", deviceID));
        Console.WriteLine(String.Format("  card number:  {0}", cardNumber));
        Console.WriteLine(String.Format("       from:    {0}", from));
        Console.WriteLine(String.Format("       to:      {0}", to));
        Console.WriteLine(String.Format("       door[1]: {0}", doors[0]));
        Console.WriteLine(String.Format("       door[2]: {0}", doors[1]));
        Console.WriteLine(String.Format("       door[3]: {0}", doors[2]));
        Console.WriteLine(String.Format("       door[4]: {0}", doors[3]));
        Console.WriteLine();
    }

    static void DeleteCard(Uhppoted u, string[] args) {
        uint deviceID = DEVICEID;
        uint cardNumber = CARD_NUMBER;

        u.DeleteCard(deviceID, cardNumber);

        Console.WriteLine(String.Format("delete-card"));
        Console.WriteLine(String.Format("  ID:           {0}", deviceID));
        Console.WriteLine(String.Format("  card number:  {0}", cardNumber));
        Console.WriteLine();
    }
}
