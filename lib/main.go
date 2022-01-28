package main

import ( 
    "C"
)

//export InterOp
func InterOp() int { 
    return 12345
}

//export InterOpX
func InterOpX(path *C.char) int { 
    return 98765
}

func main() {}


