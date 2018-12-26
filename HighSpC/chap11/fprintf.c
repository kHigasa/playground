//
// Created by andre on 2018-12-26.
//

#include <stdio.h>
#include <string.h>

int
main(int argc, char *argv[])
{
    char buf[8192];
    char *c;
    int line_no;
    FILE *fp_read, *fp_write;

    if (argc != 2) {
        printf("usage: %s [filename]\n", argv[0]);
        return -1;
    }

    fp_read = fopen(argv[1], "r");
    if (fp_read == NULL) {
        printf("Can't open file\n");
        return -1;
    }

    fp_write = fopen("out.txt", "w");
    if (fp_write == NULL) {
        printf("Can't create file\n");
        fclose(fp_read);
        return -1;
    }

    line_no = 0;
    while ((fgets(buf, sizeof(buf), fp_read)) != NULL) {
        c = strchr(buf, '\n');
        if (c != NULL) {
            *c = '\0';
        }
        if (fprintf(fp_write, "%04d: %s\n", line_no++, buf) < 0) {
            if (ferror(fp_write)) {
                printf("Error\n");
                fclose(fp_write);
                fclose(fp_read);
            }
        }
    }

    if (feof(fp_read)) {
        printf("End of file\n");
    } else if (ferror(fp_read)) {
        printf("Error\n");
    }

    fclose(fp_write);
    fclose(fp_read);

    return 0;
}
