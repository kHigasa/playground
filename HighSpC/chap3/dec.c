//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

int
main(int argc, char *argv[])
{
    int a, b;
    int answer;

    a = 1;
    b = 1;
    answer = a - -b;
    printf("answer is %d\n", answer);

    a = 1;
    b = 1;
    answer = a --- b;
    printf("answer is %d\n", answer);

    a = 1;
    b = 1;
    answer = a - -- b;
    printf("answer is %d\n", answer);

    return 0;
}