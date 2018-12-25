//
// Created by andre on 2018-12-25.
//

#include <stdio.h>

int
main(int argc, char *argv[])
{
    char one_string[] = "hello, world\n";

    printf(" one_string   : %s\n", one_string);
    printf("&one_string[0]: %s\n", &one_string[0]);
    printf("&one_string[1]: %s\n", &one_string[1]);
    printf("&one_string[2]: %s\n", &one_string[2]);
    printf("&one_string[3]: %s\n", &one_string[3]);
    printf(".\n.\n.\n");

    return 0;
}
