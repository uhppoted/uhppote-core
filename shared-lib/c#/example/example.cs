using System;
using System.Runtime.InteropServices; 

public struct GoGetDevices {
	public int N;
    public string err;
}

[StructLayout(LayoutKind.Sequential)]
public class udevice {
	public uint     ID;
	public string   address;
	public IntPtr   next;

    public udevice(uint ID, string address) {
       this.ID = ID;
       this.address = address;
       this.next = IntPtr.Zero;
    }
};

public struct UHPPOTE {
	public string bind;
	public string broadcast;
	public string listen;
	public int    timeout;  // seconds, defaults to 5 if <= 0
	public IntPtr devices;  // (optional) linked list of device address
	public bool   debug;
};

public class HelloWorld {
    [DllImport( "libuhppoted.so")]
    public static extern GoGetDevices GetDevices(ref UHPPOTE u,int N,uint[] list);

    public static void Main(string[] args) {
        Console.WriteLine ("UHPPOTE/C#");

        IntPtr p = IntPtr.Zero;

        try {
   	        udevice alpha = new udevice(405419896, "192.168.1.100");
            udevice beta = new udevice(303986753, "192.168.1.100");

            UHPPOTE u = new UHPPOTE();
            u.bind = "192.168.1.100";
            u.broadcast = "192.168.1.100:60000";
            u.listen = "192.168.1.100:60001";
            u.timeout = 2;
            u.debug = true;

            p = Marshal.AllocHGlobal(Marshal.SizeOf(alpha));
            Marshal.StructureToPtr(alpha, p, false);

            beta.next = p;

            p = Marshal.AllocHGlobal(Marshal.SizeOf(beta));
            Marshal.StructureToPtr(beta, p, false);
 
            u.devices = p;

            uint[] list = new uint[16];
            GoGetDevices rv = GetDevices(ref u,16, list);

            Console.WriteLine ("get-devices " + rv.N + "  " + rv.err);
            for (int i=0; i<rv.N; i++) {
                Console.WriteLine ("  " + list[i]);
            }
	    } finally {
            IntPtr  next;
            udevice q;

            while (p != IntPtr.Zero) {
               q = (udevice) Marshal.PtrToStructure(p, typeof(udevice));
               next = q.next;
               Marshal.FreeHGlobal(p);
               p = next;
            }
        }
    }
}
