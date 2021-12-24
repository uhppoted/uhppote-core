package main

import ( 
    "C"
)

//export GetDevices
func GetDevices(path *C.char) int { 
    return 12345
}

func main() {}
