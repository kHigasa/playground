//
// Created by andre on 2018-12-26.
//

#include <stdio.h>

#define FILE_NAME "test.dat"

int main(int argc, char **argv) {
    int array[16] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};
    size_t write_num;
    FILE *fp;

    fp = fopen(FILE_NAME, "w");
    if (fp == NULL) {
        printf("Can't create file\n");
        return -1;
    }

    write_num = fwrite(array, sizeof(int), 16, fp);
    if (write_num != 16) {
        printf("Error\n");
        fclose(fp);
        return -1;
    }

    printf("fwrite complete\n");

    fclose(fp);

    return 0;
}
