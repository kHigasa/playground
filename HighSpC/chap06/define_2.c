//
// Created by andre on 2018-12-22.
//

#define TEST 1

#if TEST == 1 // #ifdef TEST
#include <stdio.h>
int
main(int argc, char *argv[])
{
    printf("hello, world\n");

    return 0;
}
#else
#endif

// #ifndef TEST
// #else
// #endif
