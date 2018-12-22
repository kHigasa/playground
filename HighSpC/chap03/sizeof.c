//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

int main(int argc, char *argv[])
{
    char a;
    int b;
    double c;

    printf("size of literal `1' is %zu\n", sizeof(1));
    printf("size of literal `hello, world\\n' is %zu\n", sizeof("hello, world\n"));

    printf("size of variable a is %zu\n", sizeof a);
    printf("size of variable b is %zu\n", sizeof b);
    printf("size of variable c is %zu\n", sizeof c);

    printf("size of type long long is %zu\n", sizeof(long long));

    return 0;
}