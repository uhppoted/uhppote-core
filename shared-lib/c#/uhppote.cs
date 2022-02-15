using System;
using System.Runtime.InteropServices; 

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

public struct GoGetDevices {
	public int N;
    public string err;
}

public class uhppoted {
    private UHPPOTE u = new UHPPOTE();

    public uhppoted() {
    }

    public uhppoted(string bind,string broadcast,string listen,int timeout,udevice []devices,bool debug) {
        this.u.bind = bind;
        this.u.broadcast = broadcast;
        this.u.listen = listen;
        this.u.timeout = timeout;
        this.u.debug = debug;

        IntPtr p = IntPtr.Zero;
        for (int ix=0; ix<devices.Length; ix++) {
            udevice d = devices[ix];

            d.next = p;
            p = Marshal.AllocHGlobal(Marshal.SizeOf(d));

            Marshal.StructureToPtr(d, p, false);
        }

        this.u.devices = p;
    }

    ~uhppoted() {
        IntPtr p = this.u.devices;
        while (p != IntPtr.Zero) {
            udevice q = (udevice) Marshal.PtrToStructure(p, typeof(udevice));
            IntPtr next = q.next;
            Marshal.FreeHGlobal(p);
            p = next;
        }
    }

    [DllImport( "libuhppoted.so")]
    private static extern GoGetDevices GetDevices(ref UHPPOTE u,int N,uint[] list);

    public uint[] GetDevices() {
        GoGetDevices rv;
        int N = 0;
        uint[] slice;

        do {
            N += 16;
            slice = new uint[N];
            rv = GetDevices(ref this.u,N, slice);
        } while (N < rv.N);

        uint[] list = new uint[rv.N];

        Array.Copy(slice , list , list.Length);

        return list;
    }
}
