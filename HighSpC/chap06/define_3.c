//
// Created by andre on 2018-12-25.
//

#include <stdio.h>

#define SUM(a, b)    a + b
#define SUB(a, b)    a - b
#define MUL(a, b)    a * b
#define DIV(a, b)    a / b

int
main(int argc, char *argv[])
{
    int num_1;
    int num_2;
    int answer;

    num_1 = 1;
    num_2 = 2;

    answer = SUM(num_1, num_2);
    printf("answer = %d\n", answer);

    answer = SUB(num_1, num_2);
    printf("answer = %d\n", answer);

    answer = MUL(num_1, num_2);
    printf("answer = %d\n", answer);

    answer = DIV(num_1, num_2);
    printf("answer = %d\n", answer);

    return 0;
}
