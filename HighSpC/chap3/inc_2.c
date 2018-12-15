//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

int main(int argc, char *argv[])
{
    int n;

    n = 1;
    printf("n = n + ++n + n of n = %d\n", n = n + ++n + n);
    printf("n=%d\n", n);

    n = 1;
    printf("n = n + n++ + n of n = %d\n", n = n + n++ + n);
    printf("n=%d\n", n);

    return 0;
}