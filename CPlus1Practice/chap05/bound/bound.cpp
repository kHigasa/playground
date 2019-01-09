//
// Created by andre on 2019-01-09.
//

#include <iostream>
#include <assert.h>

int primes[] = {2, 3, 5, 7, 11, 13, 17};

int main()
{
    int index = 7;

    assert(index < (sizeof(primes) / sizeof(primes[0])));
    assert(index >= 0);

    std::cout << "The tenth prime is " << primes[index] << '\n';
    return (0);
}
