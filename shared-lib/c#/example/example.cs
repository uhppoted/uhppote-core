using System;

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
           controller []controllers = { new controller(405419896, "192.168.1.100"),
                                        new controller(303986753, "192.168.1.100")
                                      };

           uhppoted u = new uhppoted("192.168.1.100","192.168.1.100:60000","192.168.1.100:60001",2,controllers,true);

           switch (cmd) {
              case "get-devices":
                 GetDevices(ref u);
                 break;

              case "all":
                 GetDevices(ref u);
                 break;

              default:
                 Console.WriteLine(String.Format("  *** ERROR: invalid command ({0})",cmd));
                 break;
           }

       } catch (Exception e) {
           Console.WriteLine(String.Format("  *** ERROR: {0}",e.Message));
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
       Console.WriteLine("    help");
       Console.WriteLine();
       Console.WriteLine("  get-devices");
       Console.WriteLine("    Retrieves a list of UHPPOTE controller IDs findable on the local LAN.");
       Console.WriteLine();
       Console.WriteLine("  get-device");
       Console.WriteLine("    Retrieves the basic device information for a single UHPPOTE controller.");
       Console.WriteLine();
       Console.WriteLine("  help");
       Console.WriteLine("    Displays this information.");
       Console.WriteLine();
    }

    static void GetDevices(ref uhppoted u) {
       uint []list = u.GetDevices();

       Console.WriteLine (String.Format("get-devices ({0})",list.Length));
       for (int i=0; i<list.Length; i++) {
          Console.WriteLine (String.Format("  {0}",list[i]));
       }
       Console.WriteLine();
    }

}
