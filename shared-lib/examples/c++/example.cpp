#include <iostream>

#include "../include/uhppoted.hpp"
#include "device.hpp"

using namespace std;

void usage();

const uint32_t DEVICEID = 405419896;
const controller ALPHA = {.id = 405419896, .address = "192.168.1.100"};
const controller BETA = {.id = 303986753, .address = "192.168.1.100"};

int main(int argc, char **argv) {
  if (argc < 2) {
    cerr << endl << "*** ERROR missing command" << endl << endl;
    usage();
    return -1;
  }

  string cmd(argv[1]);
  vector<controller> controllers = {ALPHA, BETA};

  uhppoted u("192.168.1.100:0", "192.168.1.255:60000", "192.168.1.100:60001", 2,
             controllers, true);

  if (cmd == "get-devices") {
    return getDevices(u);
  } else if (cmd == "get-device") {
    return getDevice(u, DEVICEID);
  } else if (cmd == "set-address") {
    return setAddress(u, DEVICEID, "192.168.1.125", "255.255.254.0",
                      "192.168.1.10");
  } else if (cmd == "get-status") {
    return getStatus(u, DEVICEID);
  } else if (cmd == "all") {
    getDevices(u);
    getDevice(u, DEVICEID);
    setAddress(u, DEVICEID, "192.168.1.125", "255.255.254.0", "192.168.1.10");
    getStatus(u, DEVICEID);
    return 0;
  }

  cerr << endl << "*** ERROR invalid command '" << cmd << "'" << endl << endl;

  return -1;
}

void usage() {
  cout << "Usage: example <command>" << endl;
  cout << endl;
  cout << "   Supported commands:" << endl;
  cout << "      get-devices" << endl;
  cout << "      get-device" << endl;
  cout << "      set-address" << endl;
  cout << "      get-status" << endl;
  cout << endl;
}
