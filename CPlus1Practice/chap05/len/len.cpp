//
// Created by andre on 2019-01-06.
//

#include <string>
#include <iostream>

std::string line;

int main() {
    std::cout << "Enter a line:";
    std::getline(std::cin, line);

    std::cout << "The length of the line is: " << line.length() << '\n';
    return 0;
}
