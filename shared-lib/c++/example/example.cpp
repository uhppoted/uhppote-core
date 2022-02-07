#include <stdlib.h>
#include <iostream>

#include "../include/uhppote.hpp"

void usage();
extern int getDevices(uhppote&);
extern int getDevice(uhppote&);

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cerr << std::endl << "*** ERROR missing command" << std::endl << std::endl;
        usage();
        return -1;
    }

    char *cmd = argv[1];

    controller alpha = { .id=405419896, .address="192.168.1.100" };
    controller beta  = { .id=303986753, .address="192.168.1.100" };
    std::vector<controller> controllers = { alpha, beta };

    uhppote u("192.168.1.100:0","192.168.1.255:60000","192.168.1.100:60001", 2, controllers, true);

    if (strncmp(cmd,"get-devices",11) == 0) {
        return getDevices(u);
    } 

    if (strncmp(cmd,"get-device",10) == 0) {
        return getDevice(u);
    }

    if (strncmp(cmd,"all",3) == 0) {
        getDevices(u);
        getDevice(u);
    }
}

void usage() {
    std::cout << "Usage: example <command>" << std::endl;
    std::cout << std::endl;
    std::cout << "   Supported commands:" << std::endl;
    std::cout << "      get-devices" << std::endl;
    std::cout << "      get-device" << std::endl;
    std::cout << std::endl;
}
