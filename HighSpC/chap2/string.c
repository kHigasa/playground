//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

int main(int argc, char *argv[])
{
    char one_string[14];
    one_string[0] = 104;
    one_string[1] = 101;
    one_string[2] = 108;
    one_string[3] = 108;
    one_string[4] = 111;
    one_string[5] = 44;
    one_string[6] = 32;
    one_string[7] = 119;
    one_string[8] = 111;
    one_string[9] = 114;
    one_string[10] = 108;
    one_string[11] = 100;
    one_string[12] = 10;
    one_string[13] = 0;

    printf("%s", one_string);

    return 0;
}