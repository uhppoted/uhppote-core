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

public class examples {
    const uint DEVICE_ID = 405419896;
    const uint CARD_NUMBER = 8000001;
    const uint CARD_INDEX = 7;
    const uint EVENT_INDEX = 43;
    const byte DOOR = 4;

    static command[] commands = {
        new command("get-devices",
                    "Retrieves a list of UHPPOTE controller IDs findable on the local LAN.",
                    GetDevices),
        new command("get-device",
                    "Retrieves the basic device information for a single UHPPOTE controller.",
                    GetDevice),
        new command("set-address",
                    "Sets the controller IPv4 address, subnet mask and gateway address.",
                    SetAddress),
        new command("get-status",
                    "Retrieves a controller status.",
                    GetStatus),
        new command("get-time",
                    "Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss).",
                    GetTime),
        new command("set-time",
                    "Sets a controller current date/time (YYYY-MM-DD HH:mm:ss).",
                    SetTime),
        new command("get-listener",
                    "Retrieves a controller's configured event listener address.",
                    GetListener),
        new command("set-listener",
                    "Configures a controller's event listener address and port.",
                    SetListener),
        new command("get-door-control",
                    "Retrieves the control state and open delay for a controller door.",
                    GetDoorControl),
        new command("set-door-control",
                    "Sets the control mode and delay for a controller door.",
                    SetDoorControl),
        new command("get-cards",
                    "Retrieves the number of cards stored on a controller.",
                    GetCards),
        new command("get-card",
                    "Retrieves the card detail for card number from a controller.",
                    GetCard),
        new command("get-card-by-index",
                    "Retrieves the card detail for the card stored at an index on a controller.",
                    GetCardByIndex),
        new command("put-card",
                    "Adds or updates the card detail for card number stored on a controller.",
                    PutCard),
        new command("delete-card",
                    "Deletes a card from a controller.",
                    DeleteCard),
        new command("delete-cards",
                    "Deletes all cards from a controller.",
                    DeleteCards),
        new command("get-event-index",
                    "Retrieves the current event index from a controller.",
                    GetEventIndex),
        new command("set-event-index",
                    "Sets the current event index on a controller.",
                    SetEventIndex),
        new command("get-event",
                    "Retrieves the event at the index  from a controller.",
                    GetEvent),
    };

    static Controller[] controllers = { new Controller(405419896, "192.168.1.100"),
                                        new Controller(303986753, "192.168.1.100") };

    public static void Main(string[] args) {
        if (args.Length < 1) {
            usage();
            return;
        }

        string cmd = args[0];

        try {
            if (cmd == "help") {
                help();
                return;
            }

            foreach (command c in commands) {
                if (c.cmd == cmd) {
                    Uhppoted u = new Uhppoted("192.168.1.100", "192.168.1.100:60000", "192.168.1.100:60001", 2500, controllers, true);

                    c.fn(u, args);
                    return;
                }
            }

            Console.WriteLine();
            Console.WriteLine(String.Format("  *** ERROR: invalid command ({0})", cmd));
            usage();

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

    static void GetDevice(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        Device device = u.GetDevice(deviceID);

        Console.WriteLine(String.Format("get-device"));
        Console.WriteLine(String.Format("  ID:       {0}", device.ID));
        Console.WriteLine(String.Format("  IP:       {0}  {1}  {2}", device.address, device.subnet, device.gateway));
        Console.WriteLine(String.Format("  MAC:      {0}", device.MAC));
        Console.WriteLine(String.Format("  version:  {0}", device.version));
        Console.WriteLine(String.Format("  released: {0}", device.date));
        Console.WriteLine();
    }

    static void SetAddress(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        string address = "192.168.1.125";
        string subnet = "255.255.255.254";
        string gateway = "192.168.1.5";

        u.SetAddress(deviceID, address, subnet, gateway);

        Console.WriteLine(String.Format("set-address"));
        Console.WriteLine(String.Format("  ID:      {0}", deviceID));
        Console.WriteLine(String.Format("  address: {0}", address));
        Console.WriteLine(String.Format("  subnet:  {0}", subnet));
        Console.WriteLine(String.Format("  gateway: {0}", gateway));
        Console.WriteLine();
    }

    static void GetStatus(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

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

    static void GetTime(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        string datetime = u.GetTime(deviceID);

        Console.WriteLine(String.Format("get-time"));
        Console.WriteLine(String.Format("  date/time: {0}", datetime));
        Console.WriteLine();
    }

    static void SetTime(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        string datetime = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss");

        u.SetTime(deviceID, datetime);

        Console.WriteLine(String.Format("set-time"));
        Console.WriteLine(String.Format("  ID:        {0}", deviceID));
        Console.WriteLine(String.Format("  date/time: {0}", datetime));
        Console.WriteLine();
    }

    static void GetListener(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        string listener = u.GetListener(deviceID);

        Console.WriteLine(String.Format("get-listener"));
        Console.WriteLine(String.Format("  listener: {0}", listener));
        Console.WriteLine();
    }

    static void SetListener(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        string listener = "192.168.1.100:60001";

        u.SetListener(deviceID, listener);

        Console.WriteLine(String.Format("set-listener"));
        Console.WriteLine(String.Format("  ID:             {0}", deviceID));
        Console.WriteLine(String.Format("  event listener: {0}", listener));
        Console.WriteLine();
    }

    static void GetDoorControl(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        byte door = DOOR;

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

    static void SetDoorControl(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        byte door = DOOR;
        byte mode = ControlModes.NormallyOpen;
        byte delay = 9;

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

    static void GetCards(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        int cards = u.GetCards(deviceID);

        Console.WriteLine(String.Format("get-cards"));
        Console.WriteLine(String.Format("  ID:    {0}", deviceID));
        Console.WriteLine(String.Format("  cards: {0}", cards));
        Console.WriteLine();
    }

    static void GetCard(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        uint cardNumber = CARD_NUMBER;
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
        uint deviceID = DEVICE_ID;
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
        uint deviceID = DEVICE_ID;
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
        uint deviceID = DEVICE_ID;
        uint cardNumber = CARD_NUMBER;

        u.DeleteCard(deviceID, cardNumber);

        Console.WriteLine(String.Format("delete-card"));
        Console.WriteLine(String.Format("  ID:           {0}", deviceID));
        Console.WriteLine(String.Format("  card number:  {0}", cardNumber));
        Console.WriteLine();
    }

    static void DeleteCards(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        u.DeleteCards(deviceID);

        Console.WriteLine(String.Format("delete-cards"));
        Console.WriteLine(String.Format("  ID: {0}", deviceID));
        Console.WriteLine();
    }

    static void GetEventIndex(Uhppoted u, string[] args) {
        string tag = "get-event-index";
        uint deviceID = DEVICE_ID;

        uint index = u.GetEventIndex(deviceID);

        Console.WriteLine(String.Format("{0}", tag));
        Console.WriteLine(String.Format("  ID:    {0}", deviceID));
        Console.WriteLine(String.Format("  index: {0}", index));
        Console.WriteLine();
    }

    static void SetEventIndex(Uhppoted u, string[] args) {
        string tag = "set-event-index";
        uint deviceID = DEVICE_ID;
        uint index = EVENT_INDEX;

        u.SetEventIndex(deviceID, index);

        Console.WriteLine(String.Format("{0}", tag));
        Console.WriteLine(String.Format("  ID:    {0}", deviceID));
        Console.WriteLine(String.Format("  index: {0}", index));
        Console.WriteLine();
    }

    static void GetEvent(Uhppoted u, string[] args) {
        string tag = "get-event";
        uint deviceID = DEVICE_ID;
        uint index = EVENT_INDEX;

        Event evt = u.GetEvent(deviceID, index);

        Console.WriteLine(String.Format("{0}", tag));
        Console.WriteLine(String.Format("  ID:                {0}", deviceID));
        Console.WriteLine(String.Format("  event index:       {0}", evt.index));
        Console.WriteLine(String.Format("        timestamp:   {0}", evt.timestamp));
        Console.WriteLine(String.Format("        type:        {0}", evt.eventType));
        Console.WriteLine(String.Format("        granted:     {0}", evt.granted));
        Console.WriteLine(String.Format("        door:        {0}", evt.door));
        Console.WriteLine(String.Format("        direction:   {0}", evt.direction));
        Console.WriteLine(String.Format("        card number: {0}", evt.card));
        Console.WriteLine(String.Format("        reason:      {0}", evt.reason));
        Console.WriteLine();
    }
}
