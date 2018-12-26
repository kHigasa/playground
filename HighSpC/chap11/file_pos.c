//
// Created by andre on 2018-12-26.
//

#include <stdio.h>

#define FILE_NAME   "test.dat"

int main(int argc, char **argv) {
    int array[16] = {0};
    int i;
    size_t read_num;
    FILE *fp;

    fp = fopen(FILE_NAME, "r");
    if (fp == NULL) {
        printf("Can't open file\n");
        return -1;
    }

    fseek(fp, -sizeof(int), SEEK_END);
    for (i = 0; i < 16; i++) {
        read_num = fread(&array[i], sizeof(int), 1, fp);
        if (read_num != 1) {
            if (feof(fp)) {
                printf("End of file\n");
            } else if (ferror(fp)) {
                printf("Error\n");
            }
            fclose(fp);
            return -1;
        }
        fseek(fp, -(sizeof(int) * 2), SEEK_CUR);
    }

    for (i = 0; i < 16; i++) {
        printf("%d ", array[i]);
    }
    printf("\n");

    return 0;
}
