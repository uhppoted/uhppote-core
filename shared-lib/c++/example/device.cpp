#include <iostream>
#include "../include/uhppote.hpp"

int getDevices(uhppote& u) {
    try {
        auto devices = u.get_devices();
    
        std::cout << std::endl << "get-devices (" << devices.size() << ")" << std::endl;
        for (auto id : devices){
            std::cout << "   " << id << std::endl;
        }
        std::cout << std::endl;

        return 0;
    } catch (const std::exception& e) {  
        std::cout << std::endl << " *** ERROR " << e.what() << std::endl << std::endl;
    }

    return -1;
}

int getDevice(uhppote& u, uint32_t deviceID) {
    try {
        auto d = u.get_device(deviceID);
      
        std::cout << std::endl << "get-device" << std::endl;
        std::cout << "  ID:      " << d.ID << std::endl;
        std::cout << "  IP:      " << d.address << "  " << d.subnet << "  " << d.gateway << std::endl;
        std::cout << "  MAC:     " << d.MAC << std::endl;
        std::cout << "  version: " << d.version << std::endl;
        std::cout << "  date:    " << d.date << std::endl;
        std::cout << std::endl;

        return 0;
    } catch (const std::exception& e) {  
        std::cout << std::endl << " *** ERROR " << e.what() << std::endl << std::endl;
    }

    return -1;
}
