//
// Created by andre on 2019-01-10.
//

#include <iostream>
#include <string>

/********************************************************
 * tmp_name -- return a temporary file name		*
 *							*
 * Each time this function is called, a new name will	*
 * be returned.						*
 *							*
 * Returns						*
 * 	String containing the name.			*
 ********************************************************/
std::string& tmp_name()
{
    static std::string name;	// The name we are generating
    static int sequence = 0;	// Sequence number for last digit

    ++sequence;	// Move to the next file name

    name = "tmp";

    // Put in the sequence digit
    name += static_cast<char>(sequence + '0');

    return(name);
}

int main()
{
    std::string name1 = tmp_name();

    std::cout <<"Name1: " << name1 << '\n';
    return(0);
}
