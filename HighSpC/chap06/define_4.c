//
// Created by andre on 2018-12-25.
//

#include <stdio.h>

/**
 * #define SWAP(a, b) do {              \
 *                 (a) ^= (b);          \
 *                 (b) ^= (a);          \
 *                 (a) ^= (b);
 *             } while (0)              Point:←この後の；をはぶくことにより関数呼び出しがなされず繰り返しが行われない
 */
#define SWAP(a, b)   {a ^= b; b ^= a; a ^= b;}

int
main(int argc, char *argv[])
{
    int num_1;
    int num_2;

    num_1 = 1;
    num_2 = 2;

    printf("num_1 = %d, num_2 = %d\n", num_1, num_2);
    SWAP(num_1, num_2);
    printf("num_1 = %d, num_2 = %d\n", num_1, num_2);

    return 0;
}
