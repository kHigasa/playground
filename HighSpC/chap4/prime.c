//
// Created by andre on 2018-12-22.
//

#include <stdio.h>

int main(int argc, char *argv[])
{
    int i, j, end;
    int flag_not_prime;

    end = 100;

    i = 1;

    while (i <= end) {
        flag_not_prime = 0;
        j = 2;
        while (j < i) {
            if (i % j == 0) {
                flag_not_prime = 1;
                break;
            }
            j++;
        }
        if (i == 1) {
            i++;
            continue;
        }
        if (flag_not_prime == 0) {
            printf("%d\n", i);
        }
        i++;
    }
    return 0;
}