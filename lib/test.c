#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "uhppote.h"

int main(int argc, char **argv) {
    char *path = "qwerty";
    int w = GetDevices(path);

    printf("%d\n", w);

    return 0;
}