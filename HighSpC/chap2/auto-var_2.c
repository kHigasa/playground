//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

void func(void)
{
    static int count = 0;

    count++;

    printf("count = %d\n", count);

    return;
}

int main(int argc, char *argv[])
{
    func();
    func();
    func();

    return 0;
}