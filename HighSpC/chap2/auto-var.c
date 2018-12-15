//
// Created by andre on 2018-12-15.
//

#include <stdio.h>

char *func(void)
{
    static char one_string[14] = "hello, world\n";

    printf("from func: %s", one_string);

    return one_string; // 配列の先頭アドレスを呼び出し元に戻す
}

int main(int argc, char *argv[])
{
    printf("from main: %s", func());

    return 0;
}