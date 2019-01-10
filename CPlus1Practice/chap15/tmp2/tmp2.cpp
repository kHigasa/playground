//
// Created by andre on 2019-01-10.
//

#include <iostream>
#include <cstring>

/********************************************************
 * tmp_name -- return a temporary file name		*
 *							*
 * Each time this function is called, a new name will	*
 * be returned.						*
 *							*
 * Warning: There should be a warning here, but if we	*
 *	put it in we would answer the question.		*
 *							*
 * Returns						*
 * 	Pointer to the new file name.			*
 ********************************************************/
char *tmp_name()
{
    static char name[30];	// The name we are generating
    static int sequence = 0;	// Sequence number for last digit

    ++sequence;	// Move to the next file name

    strcpy(name, "tmp");

    // But in the sequence digit
    name[3] = static_cast<char>(sequence + '0');

    // End the string
    name[4] = '\0';

    return(name);
}

int main()
{
    char name1[100];		// name of a temporary file
    char name2[100];		// name of a temporary file

    strncpy(name1, tmp_name(), sizeof(name1));
    strncpy(name2, tmp_name(), sizeof(name2));

    std::cout <<"Name1: " << name1 << '\n';
    std::cout <<"Name2: " << name2 << '\n';
    return(0);
}
