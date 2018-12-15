//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

int main(int argc, char *argv[])
{
    int character = 0;
    while (character < 128) {
        printf("%c", character);
        character++;
        if ((character % 16) == 0) {
            printf("\n");
        }
    }

    return 0;
}