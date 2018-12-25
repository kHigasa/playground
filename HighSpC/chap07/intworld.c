//
// Created by andre on 2018-12-25.
//

#include <stdio.h>

int main(int argc, char *argv[])
{
    int number_array[] = {0x6c6c6568, 0x77202c6f, 0x646c726f, 0x0000000a};

    printf("%s", (char *) number_array);

    return 0;
}
