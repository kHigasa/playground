//
// Created by andre on 2019-01-06.
//

#include <string>
#include <iostream>

std::string first_name;	// First name of the author
std::string last_name;	// Last name of the author
std::string full_name;	// Full name of the author
std::wstring author; // wide string

int main()
{
    first_name = "Steve";
    last_name = "Oualline";
    full_name = first_name + "length " + first_name.length() + " " + last_name;
    author = L"ほげほげ";
    std::cout << "Full name is " << full_name << author << '\n';
    return (0);
}
