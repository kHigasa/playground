//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

int global_number;

void func_1(void)
{
    printf("from func_1: %d\n", global_number);

    return;
}

void
func_2(void)
{
    printf("from func_2: %d\n", global_number);

    return;
}

void
func_3(void)
{
    printf("from func_3: %d\n", global_number);

    return;
}

int main(int argc, char *argv[])
{
    global_number = 1;

    func_1();

    global_number = 10;

    func_2();

    global_number = 100;

    func_3();

    return 0;
}