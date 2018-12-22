//
// Created by andre on 2018-12-22.
//

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    int c;

    srand(0);

    do {
        printf("%d\n", rand() % 6 + 1);
        c = getchar();
    } while (c != 'q');

    return 0;
}