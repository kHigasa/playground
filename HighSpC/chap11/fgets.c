//
// Created by andre on 2018-12-26.
//

#include <stdio.h>
#include <string.h>

int main(int argc, char **argv) {
    char buf[8192];
    char *c;
    int line_no;
    FILE *fp;

    if (argc != 2) {
        printf("usage: %s [filename]\n", *argv);
        return -1;
    }

    fp = fopen(argv[1], "r");
    if (fp == NULL) {
        printf("Can't open file\n");
        return -1;
    }

    line_no = 0;
    while ((fgets(buf, sizeof(buf), fp)) != NULL) {
        c = strchr(buf, '\n');
        if (c != NULL) {
            *c = '\0';
        }

        printf("%04d: %s\n", line_no++, buf);
    }

    if (feof(fp)) {
        printf("End of file\n");
    } else if (ferror(fp)) {
        printf("Error\n");
    }

    fclose(fp);

    return 0;
}
