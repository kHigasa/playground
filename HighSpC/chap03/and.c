//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

int main(int argc, char *argv[])
{
    unsigned char a;
    unsigned char b;

    a = 192;
    b = 255;
    printf("first octet is %u\n", a & b);

    a = 168;
    b = 255;
    printf("first octet is %u\n", a & b);

    a = 0;
    b = 255;
    printf("first octet is %u\n", a & b);

    a = 0;
    b = 1;
    printf("first octet is %u\n", a & b);

    return 0;
}