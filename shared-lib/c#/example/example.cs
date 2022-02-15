using System;

public class example {
    public static void Main(string[] args) {
   	    controller []controllers = { new controller(405419896, "192.168.1.100"),
                                     new controller(303986753, "192.168.1.100")
                                   };

        uhppoted u = new uhppoted("192.168.1.100","192.168.1.100:60000","192.168.1.100:60001",2,controllers,true);

        try {
           uint []list = u.GetDevices();

           Console.WriteLine ("get-devices (" + list.Length + ")");
           for (int i=0; i<list.Length; i++) {
               Console.WriteLine ("  " + list[i]);
           }

       } catch (Exception e) {
           Console.WriteLine("  *** ERROR: " + e.Message);
       }
    }
}
