//
// Created by andre on 2019-01-09.
//

#include <iostream>

// Works
void inc_counter(int& counter)
{
    ++counter;
}

int main()
{
    int a_count = 0;	// Random counter

    inc_counter(a_count);
    std::cout << a_count << '\n';
    return (0);
}
