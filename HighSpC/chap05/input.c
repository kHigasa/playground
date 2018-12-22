//
// Created by andre on 2018-12-22.
//

#include <stdio.h>

int main(int argc, char *argv[])
{
    char one_string[16];

    printf("input> ");

    fgets(one_string, 16, stdin);

    printf("Your input is %s", one_string);

    return 0;
}