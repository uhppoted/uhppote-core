#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "libuhppote.h"

int main(int argc, char **argv) {
    char *path = "qwerty";
    
    printf("%lld\n", InterOp());
    printf("%lld\n", InterOpX(path));

    return 0;
}