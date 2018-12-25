//
// Created by andre on 2018-12-25.
//

#include <stdio.h>

int main(int argc, char **argv) {
    int number_array[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    int i, answer;

    for (i = 0, answer = 0; i < (sizeof(number_array) / sizeof(number_array[0])); i++) {
        answer += number_array[i];
    }
    printf("answer = %d\n", answer);

    return 0;
}