//
// Created by andre on 2018-12-26.
//

#include <stdio.h>
#include <string.h>


int
main(int argc, char *argv[])
{
    char database[] = "064-0941,ASAHIGAOKA,CHUO-KU SAPPORO-SHI,"
                      "HOKKAIDO\n060-0041,ODORIHIGASHI,CHUO-KU "
                      "SAPPORO-SHI,HOKKAIDO\n060-0042,"
                      "ODORINISHI(1-19-CHOME),CHUO-KU "
                      "SAPPORO-SHI,HOKKAIDO";
    char *p1, *p2;
    char *token;

    p1 = database;

    do {
        p2 = strchr(p1, '\n');
        if (p2 == NULL) {
            p2 = &database[strlen(database)-1];
        } else {
            *p2 = '\0';
        }
        printf("%s\n", p1);

        token = strtok(p1, ",");
        printf("code = %s\n", token);

        token = strtok(NULL, ",");
        printf("addr2 = %s\n", token);

        token = strtok(NULL, ",");
        printf("addr1 = %s\n", token);

        token = strtok(NULL, ",");
        printf("prefecture = %s\n", token);

        printf("\n");
        p1 = p2 + 1;
    } while (*p1 != '\0');

    return 0;
}
