//
// Created by andre on 2019-01-11.
//

#include <iostream>

#include "fixed_pt.h"
#include "ctype.h"

namespace fixed_pt {

/********************************************************
 * istream >> fixed_pt -- read a fixed_pt number	*
 *							*
 * Parameters						*
 *	in_file -- file to read				*
 *	number -- place to put the number		*
 *							*
 * Returns						*
 *	reference to the input file			*
 ********************************************************/
    std::istream& operator >> (std::istream& in_file, fixed_pt& number)
    {
        long int before_dp; // Part before decimal point (dp)
        char after_dp1, after_dp2;  // Part after decimal point (dp)
        char ch;		// Random character used to verify input

        number = 0.0;	// Initialize the number (just in case)

        // We only work for 2 digit fixed point numbers
        assert(fixed_exp == 100);

        // Sentry to protect the I/O
        std::istream::sentry the_sentry(in_file, true);

        if (the_sentry)
        {
            if (in_file.bad()) return (in_file);

            // Get the number that follows the whitespace
            in_file >> before_dp;

            if (in_file.bad()) return (in_file);

            in_file >> ch;	// Get first character after number

            if (in_file.bad()) return (in_file);

            // Expect a decimal point
            if (ch != '.') {
                in_file.setstate(std::ios::failbit);
                return (in_file);
            }

            in_file >> after_dp1 >> after_dp2;
            if (in_file.bad()) return (in_file);

            // Check result for validity
            if ((!isdigit(after_dp1)) || (!isdigit(after_dp2))) {
                in_file.setstate(std::ios::failbit);
                return (in_file);
            }

            // Todo make after db two digits exact
            number.value = before_dp * fixed_exp +
                           (after_dp1 - '0') * 10 +
                           (after_dp2 - '0');

        }
        else
        {
            in_file.setstate(std::ios::failbit);
        }
        return (in_file);
    }

}
