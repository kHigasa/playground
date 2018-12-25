//
// Created by andre on 2018-12-25.
//

#include <stdio.h>

int
main(int argc, char *argv[])
{
    char string_1[16] = "hello, world";
    char string_2[16] = "";
    int i;

    printf("string_2 is = %s\n", string_2);

    for (i = 0; i < sizeof(string_2)-1 && string_1[i] != '\0'; i++) {
        string_2[i] = string_1[i];
    }
    string_2[i] = '\0';

    printf("string_2 is = %s\n", string_2);

    return 0;
}
