//
// Created by andre on 2019-01-09.
//

#include <iostream>
#include <cstring>

char name[50]; // First name of someone

int main()
{
    std::strcpy(name, "Sam");
    std::cout << "The name is " << name << '\n';
    return (0);
}
