#include <iostream>
#include "../include/uhppote.hpp"

int getDevices(uhppote& u) {
    std::vector<unsigned long> devices;

    if (u.get_devices(devices) < 0) {
        std::cerr << "ERROR " << u.errmsg() << std::endl;
        return -1;
    } 

    std::cout << std::endl << "get-devices (" << devices.size() << ")" << std::endl;
    for (auto id : devices){
        std::cout << "   " << id << std::endl;
    }
    std::cout << std::endl;

    return 0;
}

int getDevice(uhppote& u) {
    struct device d;

    if (u.get_device(405419896, d) != 0) {
        std::cerr << "ERROR " << u.errmsg() << std::endl;
        return -1;
    } 
      
    std::cout << std::endl << "get-device" << std::endl;
    std::cout << "  ID:      " << d.ID << std::endl;
    std::cout << "  IP:      " << d.address << "  " << d.subnet << "  " << d.gateway << std::endl;
    std::cout << "  MAC:     " << d.MAC << std::endl;
    std::cout << "  version: " << d.version << std::endl;
    std::cout << "  date:    " << d.date << std::endl;
    std::cout << std::endl;

    return 0;
}
