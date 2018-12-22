//
// Created by andre on 2018-12-22.
//

#include <stdio.h>

int
main(int argc, char *argv[])
{
    printf("%#o\n", 8);
    printf("%04d\n", 10);
    printf("% 4d\n", 10);

    printf("|%-4d|%-4d|\n|%4d|%4d|\n", 1, 2, 1, 2);

    printf("%+d\n", 10);
    printf("%+d\n", -10);

    printf("%.4d\n", 10);
    printf("%.5s\n", "hello, world\n");

    printf("%hhu\n", 255);
    printf("%hhu\n", 256);
    printf("%hhu\n", 257);

    printf("%e\n", 0.1);
    printf("%f\n", 0.1);

    printf("%%\n");

    return 0;
}
