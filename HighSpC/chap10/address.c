//
// Created by andre on 2018-12-26.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHARACTER    (256)

struct tag_address {
    int no;
    char name[MAX_CHARACTER];
    char addr[MAX_CHARACTER];
    struct tag_address *next;
};

struct tag_address *address = NULL;

int enter(int, const char *, const char *);
struct tag_address *find(int);
int delete(int);
void show_all(void);
void delete_all(void);

int main(int argc, char **argv) {
    struct tag_address *address_data;
    char in_buf[8];
    char in_no[MAX_CHARACTER];
    char in_addr[MAX_CHARACTER];
    char in_name[MAX_CHARACTER];
    char *c;
    int no, menu_no;

    do {
        printf("Input 1[ret] to enter, 2[ret] to search, 3[ret] to delete, 4[ret] to all, 9[ret] to quit.\n");
        printf("Input menu number> ");
        fgets(in_buf, sizeof(in_buf), stdin);
        menu_no = strtol(in_buf, NULL, 10);
        switch (menu_no) {
            case 1:
                printf("Input number> ");
                fgets(in_no, sizeof(in_no), stdin);
                no = strtol(in_no, NULL, 10);

                printf("Input name> ");
                fgets(in_name, sizeof(in_name), stdin);
                c = strchr(in_name, '\n');
                if (c != NULL) {
                    *c = '\0';
                }

                printf("Input address> ");
                fgets(in_addr, sizeof(in_addr), stdin);
                c = strchr(in_addr, '\n');
                if (c != NULL) {
                    *c = '\0';
                }

                if (enter(no, in_name, in_addr) == -1) {
                    printf("error\n\n\n");
                } else {
                    printf("entered\n\n\n");
                }
                break;
            case 2:
                printf("Input number> ");
                fgets(in_no, sizeof(in_no), stdin);
                no = strtol(in_no, NULL, 10);

                address_data = find(no);
                if (address_data == NULL) {
                    printf("no data\n\n\n");
                } else {
                    printf("no = %d\nname = %s\naddress = %s\n\n\n", no, address_data->name, address_data->addr);
                }
                break;
            case 3:
                printf("Input number> ");
                fgets(in_no, sizeof(in_no), stdin);
                no = strtol(in_no, NULL, 10);

                if (delete(no) == -1) {
                    printf("no data\n\n\n");
                } else {
                    printf("deleted\n\n\n");
                }
                break;
            case 4:
                show_all();
                break;
            default:
                break;
        }
    } while (menu_no != 9);

    delete_all();

    return 0;
}

int enter(int no, const char *name, const char *addr) {
    struct tag_address *ptr_now, *ptr_before, *ptr_new;

    ptr_now = address;
    ptr_before = NULL;

    while (ptr_now != NULL) {
        if (no == ptr_now->no) {
            printf("already exist\n");
            return -1;
        }

        if (no < ptr_now->no) {
            break;
        }

        ptr_before = ptr_now;
        ptr_now = ptr_now->next;
    }

    ptr_new = malloc(sizeof(struct tag_address));
    if (ptr_new == NULL) {
        printf("Memory allocation error!\n");
        return -1;
    }
    ptr_new->no = no;
    strncpy(ptr_new->name, name, sizeof(ptr_new->name) - 1);
    ptr_new->name[sizeof(ptr_new->name) - 1] = '\0';
    strncpy(ptr_new->addr, addr, sizeof(ptr_new->addr) - 1);
    ptr_new->addr[sizeof(ptr_new->addr) - 1] = '\0';


    if (address == NULL) {
        address = ptr_new;
    }

    ptr_new->next = ptr_now;

    if (ptr_before != NULL) {
        ptr_before->next = ptr_new;
    }

    if (address->no > ptr_new->no) {
        address = ptr_new;
    }

    return 0;
}

struct tag_address *
find(int no)
{
    struct tag_address *ptr;

    ptr = address;

    while (ptr != NULL) {
        if (no == ptr->no) {
            return ptr;
        }
        if (no < ptr->no) {
            break;
        }

        ptr = ptr->next;
    }

    return NULL;
}

int
delete(int no)
{
    struct tag_address *ptr_now, *ptr_before;

    ptr_now = address;

    if (ptr_now == NULL) {
        return -1;
    }

    while (ptr_now != NULL) {
        if (no == ptr_now->no) {
            break;
        }

        if (no < ptr_now->no) {
            return -1;
        }

        ptr_before = ptr_now;
        ptr_now = ptr_now->next;
    }
    if (ptr_now == NULL) {
    return -1;
}

    if (address == ptr_now) {
        address = ptr_now->next;
    } else {
        ptr_before->next = ptr_now->next;
    }

    free(ptr_now);

    return 0;
}

void
show_all(void)
{
    struct tag_address *ptr_now;

    ptr_now = address;

    while (ptr_now != NULL) {
        printf("no = %d\nname = %s\naddress = %s\n\n\n", ptr_now->no, ptr_now->name, ptr_now->addr);
        ptr_now = ptr_now->next;
    }
}

void
delete_all(void)
{
    struct tag_address *ptr_now, *ptr_before;

    ptr_now = address;

    while (ptr_now != NULL) {
        ptr_before = ptr_now;
        ptr_now = ptr_now->next;
        free(ptr_before);
    }
}
