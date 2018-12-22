//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

int main(int argc, char *argv[])
{
    int n;

    n = 0;
    printf("pre inc n=%d", ++n);
    printf("n=%d", n);

    n = 0;
    printf("post inc n=%d", n++);
    printf("n=%d", n);

    return 0;
}