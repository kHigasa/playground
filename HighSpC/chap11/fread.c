//
// Created by andre on 2018-12-26.
//

#include <stdio.h>

#define FILE_NAME   "test.dat"

int main(int argc, char **argv) {
    int array[16];
    int i;
    size_t read_num;
    FILE *fp;

    fp = fopen(FILE_NAME, "r");
    if (fp == NULL) {
        printf("Can't open file\n");
        return -1;
    }

    read_num = fread(array, sizeof(int), 16, fp);
    if (read_num != 16) {
        if (feof(fp)) {
            printf("End of file\n");
        } else if (ferror(fp)) {
            printf("Error\n");
        }
        fclose(fp);
        return -1;
    }

    for (i = 0; i < 16; i++) {
        printf("%d ", array[i]);
    }
    printf("\nfread complete\n");

    fclose(fp);

    return 0;
}
