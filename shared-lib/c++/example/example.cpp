#include <iostream>

#include "../include/uhppote.hpp"

void usage();
extern int getDevices(uhppote&);
extern int getDevice(uhppote&, uint32_t);

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cerr << std::endl << "*** ERROR missing command" << std::endl << std::endl;
        usage();
        return -1;
    }

    std::string cmd(argv[1]);

    controller alpha = { .id=405419896, .address="192.168.1.100" };
    controller beta  = { .id=303986753, .address="192.168.1.100" };
    std::vector<controller> controllers = { alpha, beta };

    uhppote u("192.168.1.100:0","192.168.1.255:60000","192.168.1.100:60001", 2, controllers, true);
 
    try {
        if (cmd == "get-devices") {
            return getDevices(u);
        } 
    
        if (cmd == "get-device") {
            return getDevice(u, 405419896);
        }

        if (cmd == "all") {
            getDevices(u);
            getDevice(u, 405419896);
        }

        return 0;
    } catch (const std::exception& e) {  
        return -1;
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
