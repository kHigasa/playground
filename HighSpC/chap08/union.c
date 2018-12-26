//
// Created by andre on 2018-12-26.
//

#include <stdio.h>

union tag_world {
    int num[4];
    char one_string[16];
};

int
main(int argc, char *argv[])
{
    union tag_world world;

    char one_string[] = "hello, world\n";
    int i;

    for (i = 0; one_string[i] != '\0'; i++) {
        world.one_string[i] = one_string[i];
    }
    world.one_string[i] = one_string[i];

    printf("world.one_string is = %s", world.one_string);
    printf("world.num[] is = 0x%08x, 0x%08x, 0x%08x, 0x%08x\n",
           world.num[0], world.num[1], world.num[2], world.num[3]);

    return 0;
}
