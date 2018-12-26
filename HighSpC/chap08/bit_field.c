//
// Created by andre on 2018-12-26.
//

#include <stdio.h>

union tag_byte_union {
    unsigned char one_byte;
    struct tag_bit_field {
        unsigned b1: 1;
        unsigned b2: 1;
        unsigned b3: 1;
        unsigned b4: 1;
        unsigned b5: 1;
        unsigned b6: 1;
        unsigned b7: 1;
        unsigned b8: 1;
    } bit_field;
};

int
main(int argc, char *argv[])
{
    union tag_byte_union byte_union;

    byte_union.one_byte = 7;

    printf("%u%u%u%u%u%u%u%u\n",
           byte_union.bit_field.b8,
           byte_union.bit_field.b7,
           byte_union.bit_field.b6,
           byte_union.bit_field.b5,
           byte_union.bit_field.b4,
           byte_union.bit_field.b3,
           byte_union.bit_field.b2,
           byte_union.bit_field.b1);

    return 0;
}
