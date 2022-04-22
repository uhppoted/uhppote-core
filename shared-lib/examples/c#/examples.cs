using System;
using static System.Console;
using static System.String;

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
    const byte PROFILE_ID = 29;

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
        new command("open-door",
                    "Remotely opens a controller door.",
                    OpenDoor),
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
                    "Retrieves the event at the index from a controller.",
                    GetEvent),
        new command("record-special-events",
                    "Enables/disables recording additional events for a controller.",
                    RecordSpecialEvents),
        new command("get-time-profile",
                    "Retrieves a time profile from a controller.",
                    GetTimeProfile),
        new command("set-time-profile",
                    "Adds or updates a time profile on a controller.",
                    GetTimeProfile),
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

            WriteLine();
            WriteLine(Format("  *** ERROR: invalid command ({0})", cmd));
            usage();

        } catch (Exception e) {
            WriteLine(Format("  *** ERROR: {0}", e.Message));
        }
    }

    static void usage() {
        WriteLine();
        WriteLine("  Usage: mono example.exe <command>");
        WriteLine();
        WriteLine("  Supported commands");

        foreach (command c in commands) {
            WriteLine("    {0}", c.cmd);
        }

        WriteLine();
    }

    static void help() {
        WriteLine();
        WriteLine("  Usage: mono example.exe <command>");
        WriteLine();
        WriteLine("  Commands");

        foreach (command c in commands) {
            WriteLine("    {0,-17}  {1}", c.cmd, c.help);
        }

        WriteLine();
    }

    static void GetDevices(Uhppoted u, string[] args) {
        uint[] list = u.GetDevices();

        WriteLine(Format("get-devices ({0})", list.Length));
        for (int i = 0; i < list.Length; i++) {
            WriteLine(Format("  {0}", list[i]));
        }
        WriteLine();
    }

    static void GetDevice(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        Device device = u.GetDevice(deviceID);

        WriteLine(Format("get-device"));
        WriteLine(Format("  ID:       {0}", device.ID));
        WriteLine(Format("  IP:       {0}  {1}  {2}", device.address, device.subnet, device.gateway));
        WriteLine(Format("  MAC:      {0}", device.MAC));
        WriteLine(Format("  version:  {0}", device.version));
        WriteLine(Format("  released: {0}", device.date));
        WriteLine();
    }

    static void SetAddress(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        string address = "192.168.1.125";
        string subnet = "255.255.255.254";
        string gateway = "192.168.1.5";

        u.SetAddress(deviceID, address, subnet, gateway);

        WriteLine(Format("set-address"));
        WriteLine(Format("  ID:      {0}", deviceID));
        WriteLine(Format("  address: {0}", address));
        WriteLine(Format("  subnet:  {0}", subnet));
        WriteLine(Format("  gateway: {0}", gateway));
        WriteLine();
    }

    static void GetStatus(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        Status status = u.GetStatus(deviceID);

        WriteLine(Format("get-status"));
        WriteLine(Format("  ID:        {0}", status.ID));
        WriteLine(Format("  timestamp: {0}", status.sysdatetime));
        WriteLine(Format("  doors:     {0} {1} {2} {3}", status.doors[0], status.doors[1], status.doors[2], status.doors[3]));
        WriteLine(Format("  buttons:   {0} {1} {2} {3}", status.buttons[0], status.buttons[1], status.buttons[2], status.buttons[3]));
        WriteLine(Format("  relays:    {0:X}", status.relays));
        WriteLine(Format("  inputs:    {0:X}", status.inputs));
        WriteLine(Format("  syserror:  {0}", status.syserror));
        WriteLine(Format("  info:      {0:X}", status.info));
        WriteLine(Format("  seqno:     {0}", status.seqno));
        WriteLine(Format("  event timestamp: {0}", status.evt.timestamp));
        WriteLine(Format("        index:     {0}", status.evt.index));
        WriteLine(Format("        type:      {0}", status.evt.eventType));
        WriteLine(Format("        granted:   {0}", status.evt.granted));
        WriteLine(Format("        door:      {0}", status.evt.door));
        WriteLine(Format("        direction: {0}", status.evt.direction));
        WriteLine(Format("        card:      {0}", status.evt.card));
        WriteLine(Format("        reason:    {0}", status.evt.reason));

        WriteLine();
    }

    static void GetTime(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        string datetime = u.GetTime(deviceID);

        WriteLine(Format("get-time"));
        WriteLine(Format("  date/time: {0}", datetime));
        WriteLine();
    }

    static void SetTime(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        string datetime = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss");

        u.SetTime(deviceID, datetime);

        WriteLine(Format("set-time"));
        WriteLine(Format("  ID:        {0}", deviceID));
        WriteLine(Format("  date/time: {0}", datetime));
        WriteLine();
    }

    static void GetListener(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        string listener = u.GetListener(deviceID);

        WriteLine(Format("get-listener"));
        WriteLine(Format("  listener: {0}", listener));
        WriteLine();
    }

    static void SetListener(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        string listener = "192.168.1.100:60001";

        u.SetListener(deviceID, listener);

        WriteLine(Format("set-listener"));
        WriteLine(Format("  ID:             {0}", deviceID));
        WriteLine(Format("  event listener: {0}", listener));
        WriteLine();
    }

    static void GetDoorControl(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        byte door = DOOR;

        DoorControl control = u.GetDoorControl(deviceID, door);

        WriteLine(Format("get-door-control"));
        WriteLine(Format("  ID:      {0}", deviceID));
        WriteLine(Format("  door:    {0}", door));

        switch (control.mode) {
        case ControlModes.NormallyOpen:
            WriteLine(Format("  mode:    {0}", "normally open"));
            break;

        case ControlModes.NormallyClosed:
            WriteLine(Format("  mode:    {0}", "normally closed"));
            break;

        case ControlModes.Controlled:
            WriteLine(Format("  mode:    {0}", "controlled"));
            break;

        default:
            WriteLine(Format("  mode:    {0}", "???"));
            break;
        }

        WriteLine(Format("  delay:   {0}", control.delay));
        WriteLine();
    }

    static void SetDoorControl(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        byte door = DOOR;
        byte mode = ControlModes.NormallyOpen;
        byte delay = 9;

        u.SetDoorControl(deviceID, door, mode, delay);

        WriteLine(Format("set-door-control"));
        WriteLine(Format("  ID:      {0}", deviceID));
        WriteLine(Format("  door:    {0}", door));

        switch (mode) {
        case ControlModes.NormallyOpen:
            WriteLine(Format("  mode:    {0}", "normally open"));
            break;

        case ControlModes.NormallyClosed:
            WriteLine(Format("  mode:    {0}", "normally closed"));
            break;

        case ControlModes.Controlled:
            WriteLine(Format("  mode:    {0}", "controlled"));
            break;

        default:
            WriteLine(Format("  mode:    {0}", "???"));
            break;
        }

        WriteLine(Format("  delay:   {0}", delay));
        WriteLine();
    }

    static void OpenDoor(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        byte door = DOOR;

        u.OpenDoor(deviceID, door);

        WriteLine(Format("open-door"));
        WriteLine(Format("  ID:      {0}", deviceID));
        WriteLine(Format("  door:    {0}", door));
        WriteLine();
    }

    static void GetCards(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        int cards = u.GetCards(deviceID);

        WriteLine(Format("get-cards"));
        WriteLine(Format("  ID:    {0}", deviceID));
        WriteLine(Format("  cards: {0}", cards));
        WriteLine();
    }

    static void GetCard(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        uint cardNumber = CARD_NUMBER;
        Card card = u.GetCard(deviceID, cardNumber);

        WriteLine(Format("get-card"));
        WriteLine(Format("  ID:           {0}", deviceID));
        WriteLine(Format("  card number:  {0}", card.cardNumber));
        WriteLine(Format("       from:    {0}", card.from));
        WriteLine(Format("       to:      {0}", card.to));
        WriteLine(Format("       door[1]: {0}", card.doors[0]));
        WriteLine(Format("       door[2]: {0}", card.doors[1]));
        WriteLine(Format("       door[3]: {0}", card.doors[2]));
        WriteLine(Format("       door[4]: {0}", card.doors[3]));
        WriteLine();
    }

    static void GetCardByIndex(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        uint index = CARD_INDEX;

        Card card = u.GetCardByIndex(deviceID, index);

        WriteLine(Format("get-card-by-index"));
        WriteLine(Format("  ID:           {0}", deviceID));
        WriteLine(Format("  index:        {0}", index));
        WriteLine(Format("  card number:  {0}", card.cardNumber));
        WriteLine(Format("       from:    {0}", card.from));
        WriteLine(Format("       to:      {0}", card.to));
        WriteLine(Format("       door[1]: {0}", card.doors[0]));
        WriteLine(Format("       door[2]: {0}", card.doors[1]));
        WriteLine(Format("       door[3]: {0}", card.doors[2]));
        WriteLine(Format("       door[4]: {0}", card.doors[3]));
        WriteLine();
    }

    static void PutCard(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        uint cardNumber = CARD_NUMBER;
        string from = "2022-01-01";
        string to = "2022-12-31";
        byte[] doors = { 0, 1, 31, 75 };

        u.PutCard(deviceID, cardNumber, from, to, doors);

        WriteLine(Format("put-card"));
        WriteLine(Format("  ID:           {0}", deviceID));
        WriteLine(Format("  card number:  {0}", cardNumber));
        WriteLine(Format("       from:    {0}", from));
        WriteLine(Format("       to:      {0}", to));
        WriteLine(Format("       door[1]: {0}", doors[0]));
        WriteLine(Format("       door[2]: {0}", doors[1]));
        WriteLine(Format("       door[3]: {0}", doors[2]));
        WriteLine(Format("       door[4]: {0}", doors[3]));
        WriteLine();
    }

    static void DeleteCard(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;
        uint cardNumber = CARD_NUMBER;

        u.DeleteCard(deviceID, cardNumber);

        WriteLine(Format("delete-card"));
        WriteLine(Format("  ID:           {0}", deviceID));
        WriteLine(Format("  card number:  {0}", cardNumber));
        WriteLine();
    }

    static void DeleteCards(Uhppoted u, string[] args) {
        uint deviceID = DEVICE_ID;

        u.DeleteCards(deviceID);

        WriteLine(Format("delete-cards"));
        WriteLine(Format("  ID: {0}", deviceID));
        WriteLine();
    }

    static void GetEventIndex(Uhppoted u, string[] args) {
        string tag = "get-event-index";
        uint deviceID = DEVICE_ID;

        uint index = u.GetEventIndex(deviceID);

        WriteLine(Format("{0}", tag));
        WriteLine(Format("  ID:    {0}", deviceID));
        WriteLine(Format("  index: {0}", index));
        WriteLine();
    }

    static void SetEventIndex(Uhppoted u, string[] args) {
        string tag = "set-event-index";
        uint deviceID = DEVICE_ID;
        uint index = EVENT_INDEX;

        u.SetEventIndex(deviceID, index);

        WriteLine(Format("{0}", tag));
        WriteLine(Format("  ID:    {0}", deviceID));
        WriteLine(Format("  index: {0}", index));
        WriteLine();
    }

    static void GetEvent(Uhppoted u, string[] args) {
        string tag = "get-event";
        uint deviceID = DEVICE_ID;
        uint index = EVENT_INDEX;

        Event evt = u.GetEvent(deviceID, index);

        WriteLine(Format("{0}", tag));
        WriteLine(Format("  ID:                {0}", deviceID));
        WriteLine(Format("  event index:       {0}", evt.index));
        WriteLine(Format("        timestamp:   {0}", evt.timestamp));
        WriteLine(Format("        type:        {0}", evt.eventType));
        WriteLine(Format("        granted:     {0}", evt.granted));
        WriteLine(Format("        door:        {0}", evt.door));
        WriteLine(Format("        direction:   {0}", evt.direction));
        WriteLine(Format("        card number: {0}", evt.card));
        WriteLine(Format("        reason:      {0}", evt.reason));
        WriteLine();
    }

    static void RecordSpecialEvents(Uhppoted u, string[] args) {
        string tag = "record-special-events";
        uint deviceID = DEVICE_ID;
        bool enabled = true;

        u.RecordSpecialEvents(deviceID, enabled);

        WriteLine(Format("{0}", tag));
        WriteLine(Format("  ID:      {0}", deviceID));
        WriteLine(Format("  enabled: {0}", enabled));
        WriteLine();
    }

    static void GetTimeProfile(Uhppoted u, string[] args) {
        string tag = "get-time-profile";
        uint deviceID = DEVICE_ID;
        byte profileID = PROFILE_ID;

        TimeProfile profile = u.GetTimeProfile(deviceID, profileID);

        WriteLine(Format("{0}", tag));
        WriteLine(Format("  ID:                   {0}", deviceID));
        WriteLine(Format("  profile ID:           {0}", profile.ID));
        WriteLine(Format("  linked profile:       {0}", profile.linked));
        WriteLine(Format("  enabled from:         {0}", profile.from));
        WriteLine(Format("          to:           {0}", profile.to));
        WriteLine(Format("  enabled on Monday:    {0}", profile.monday));
        WriteLine(Format("             Tuesday:   {0}", profile.tuesday));
        WriteLine(Format("             Wednesday: {0}", profile.wednesday));
        WriteLine(Format("             Thursday:  {0}", profile.thursday));
        WriteLine(Format("             Friday:    {0}", profile.friday));
        WriteLine(Format("             Saturday:  {0}", profile.saturday));
        WriteLine(Format("             Sunday:    {0}", profile.sunday));
        WriteLine(Format("  segment 1:            {0}-{1}", profile.segment1start, profile.segment1end));
        WriteLine(Format("  segment 2:            {0}-{1}", profile.segment2start, profile.segment2end));
        WriteLine(Format("  segment 3:            {0}-{1}", profile.segment3start, profile.segment3end));
        WriteLine();
    }

    static void SetTimeProfile(Uhppoted u, string[] args) {
        string tag = "set-time-profile";
        uint deviceID = DEVICE_ID;
        TimeProfile profile = new TimeProfile(PROFILE_ID, 71, "2022-02-01", "2022-06-30",
                                              true, false, true, true, false, false, true,
                                              "08:30", "11:30",
                                              "", "",
                                              "", "18:00");

        u.SetTimeProfile(deviceID, profile);

        WriteLine(Format("{0}", tag));
        WriteLine(Format("  ID:                   {0}", deviceID));
        WriteLine(Format("  profile ID:           {0}", profile.ID));
        WriteLine(Format("  linked profile:       {0}", profile.linked));
        WriteLine(Format("  enabled from:         {0}", profile.from));
        WriteLine(Format("          to:           {0}", profile.to));
        WriteLine(Format("  enabled on Monday:    {0}", profile.monday));
        WriteLine(Format("             Tuesday:   {0}", profile.tuesday));
        WriteLine(Format("             Wednesday: {0}", profile.wednesday));
        WriteLine(Format("             Thursday:  {0}", profile.thursday));
        WriteLine(Format("             Friday:    {0}", profile.friday));
        WriteLine(Format("             Saturday:  {0}", profile.saturday));
        WriteLine(Format("             Sunday:    {0}", profile.sunday));
        WriteLine(Format("  segment 1:            {0}-{1}", profile.segment1start, profile.segment1end));
        WriteLine(Format("  segment 2:            {0}-{1}", profile.segment2start, profile.segment2end));
        WriteLine(Format("  segment 3:            {0}-{1}", profile.segment3start, profile.segment3end));
        WriteLine();
    }
}
