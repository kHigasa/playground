//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

int main(int argc, char *argv[])
{
    int a, b, c;
    int value;

    value = (a = 1 + 1, b = 2 + 2, c = 3 + 3);

    printf("a is %d\n", a);
    printf("b is %d\n", b);
    printf("c is %d\n", c);

    printf("value of experssion is %d\n", value);

    return 0;
}