//
// Created by andre on 2019-01-06.
//

#include <iostream>

float data[5];	// data to average and total
float total;	// the total of the data items
float average;	// average of the items

int main()
{
    data[0] = 34.0;
    data[1] = 27.0;
    data[2] = 46.5;
    data[3] = 82.0;
    data[4] = 22.0;

    total = data[0] + data[1] + data[2] + data[3] + data[4];
    average =  total / 5.0;
    std::cout << "Total " << total << " Average " << average << '\n';
    return (0);
}
