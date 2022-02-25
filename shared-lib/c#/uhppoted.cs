using System;
using System.Runtime.InteropServices; 
using System.Text;

namespace uhppoted {
   public class Controller {
       public uint     ID;
       public string   address;
   
       public Controller(uint ID, string address) {
          this.ID = ID;
          this.address = address;
       }
   };
   
   public class UhppotedException : Exception {
       public UhppotedException(string message): base(message) {}
   };
   
   public class Device {
       public uint   ID;
       public string address;
       public string subnet;
       public string gateway;
       public string MAC;
       public string version;
       public string date;
   
       public Device(uint ID, string address,string subnet,string gateway,string MAC,string version,string date) {
           this.ID = ID;
           this.address = address;
           this.subnet = subnet;
           this.gateway = gateway;
           this.MAC = MAC;
           this.version = version;
           this.date = date;
       }
   };
   
   public class Uhppoted: IDisposable {
       private UHPPOTE u = new UHPPOTE();
   
       public Uhppoted() {
       }
   
       public Uhppoted(string bind,string broadcast,string listen,int timeout,Controller []controllers,bool debug) {
           this.u.bind = bind;
           this.u.broadcast = broadcast;
           this.u.listen = listen;
           this.u.timeout = timeout;
           this.u.debug = debug;
   
           IntPtr p = IntPtr.Zero;
           for (int ix=0; ix<controllers.Length; ix++) {
               Controller c = controllers[ix];
               udevice d;
   
               d.ID = c.ID;
               d.address = c.address;
               d.next = p;
   
               p = Marshal.AllocHGlobal(Marshal.SizeOf(d));
   
               Marshal.StructureToPtr(d, p, false);
           }
   
           this.u.devices = p;
       }
   
       ~Uhppoted() {
           dispose();
       }
   
       public void Dispose() {
           dispose();
           GC.SuppressFinalize(this);
       }
   
       private void dispose() {
           IntPtr p = this.u.devices;
           while (p != IntPtr.Zero) {
               udevice q = (udevice) Marshal.PtrToStructure(p, typeof(udevice));
               IntPtr next = q.next;
               Marshal.FreeHGlobal(p);
               p = next;
           }
       }
   
       [DllImport( "libuhppoted.so")]
       private static extern string GetDevices(ref UHPPOTE u,ref int N,uint[] list);
   
       public uint[] GetDevices() {
           int N = 0;
           int count = N;
           uint[] slice;
   
           do {
               N += 16;
               count = N;
               slice = new uint[N];

               string err = GetDevices(ref this.u, ref count, slice);
               if (err != null && err != "") {
                  throw new UhppotedException(err);
               }
           } while (N < count);
   
           uint[] list = new uint[count];
   
           Array.Copy(slice,list ,list.Length);
   
           return list;
       }
   
       [DllImport( "libuhppoted.so")]
       private static extern string GetDeviceX (ref UHPPOTE u,uint deviceID,ref GoDevice device);
   
       public Device GetDevice(uint deviceID) {
           GoDevice device = new GoDevice();

           string err = GetDeviceX(ref this.u, deviceID, ref device);
           if (err != null && err != "") {
               throw new UhppotedException(err);
           }
   
           return new Device(device.ID,
                             device.address,
                             device.subnet,
                             device.gateway,
                             device.MAC,
                             device.version,
                             device.date);
       }
   }
   
   struct udevice {
       public uint     ID;
       public string   address;
       public IntPtr   next;
   };
   
   struct UHPPOTE {
       public string bind;
       public string broadcast;
       public string listen;
       public int    timeout;  // seconds, defaults to 5 if <= 0
       public IntPtr devices;  // (optional) linked list of device address
       public bool   debug;
   };

   struct GoDevice {
       public uint   ID;
       public string address;
       public string subnet;
       public string gateway;
       public string MAC;
       public string version;
       public string date;
   };
   
   // Return value for interop function GetDevices - for some reason the compiler thinks it's never initialised
   // Ref. https://stackoverflow.com/questions/3820985/suppressing-is-never-used-and-is-never-assigned-to-warnings-in-c-sharp/3821035#3821035
   #pragma warning disable 0649
   #pragma warning restore 0649
}
