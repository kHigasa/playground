//
// Created by andre on 2018-12-25.
//

#include <stdio.h>

int main(int argc, char **argv) {
    double double_array[2] = {0.1, 0.2};
    double *ptr;

    ptr = double_array;
    printf("*(ptr + 0) is %f\n", *(ptr + 0)); // printf("ptr[0] is %f\n", ptr[0]);
    printf("*(ptr + 1) is %f\n", *(ptr + 1));
    printf("double_array[0] is %f\n", double_array[0]); // printf("*(double_array + 0) is %f\n", *(double_array + 0));
    printf("double_array[1] is %f\n", double_array[1]);

    return 0;
}
