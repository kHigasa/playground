//
// Created by andre on 2018-12-26.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
    char *one_string, *new_string;

    one_string = malloc(0xffffffffffffffff);
    if (one_string == NULL) {
        printf("Memory allocation error!\n");
    }
    printf("one_string=%p\n", one_string);
    free(one_string);

    one_string = calloc(14, 1);
    if (one_string == NULL) {
        printf("Memory allocation error!\n");
        return -1;
    }
    printf("one_string=%p\n", one_string);

    strncpy(one_string, "hello, world\n", 14 - 1);
    one_string[14 - 1] = '\0';

    printf("%s", one_string);

    new_string = realloc(one_string, 32);
    if (new_string == NULL) {
        printf("Memory allocation error!\n");
        free(one_string);
        return -1;
    }
    printf("new_string=%p\n", new_string);
    one_string = new_string; //Point:元のポインタ変数に代入し直すことで有効なメモリ領域を参照するようにする

    strncpy(one_string, "hello, world\nHELLO, WORLD\n", 32 - 1);
    one_string[32 - 1] = '\0';

    printf("%s", one_string);

    printf("one_string=%p\n", one_string);
    free(one_string);
    printf("one_string=%p\n", one_string);

    return 0;
}