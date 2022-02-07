#include <iostream>

#include "../include/uhppote.hpp"

int getDevices(uhppote& u) {
    unsigned long *devices = NULL;
    int N;

    if (u.get_devices(&devices, &N) < 0) {
        std::cerr << "ERROR " << u.errmsg() << std::endl;
        return -1;
    } 

    std::cout << std::endl << "get-devices (" << N << ")" << std::endl;
    if (N > 0 && devices != NULL) {
        for (int i=0; i<N; i++) {
            std::cout << "   " << devices[i] << std::endl;
        }
    }
    std::cout << std::endl;

    free(devices);

    return 0;
}

int getDevice(uhppote& u) {
    struct device d;

    if (u.get_device(405419896, &d) != 0) {
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
