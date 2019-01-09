//
// Created by andre on 2019-01-09.
//

#include <iostream>

int  result;    // the result of calculations
char open_char; // operator the user specified
int  value;     // value specified after the operator

int main()
{
    result = 0; // initialize the result

    // loop forever (or until break reached)
    while (true) {
        std::cout << "Enter operator and number: ";

        std::cin >> open_char >> value;

        if ((open_char == 'q') || (open_char == 'Q'))
            break;

        switch (open_char) {
            case '+':
                result += value;
                break;
            case '-':
                result -= value;
                break;
            case '*':
                result *= value;
                break;
            case '/':
                if (value == 0) {
                    std::cout << "Error: Divide by zero\n";
                    std::cout << "    operation ignored\n";
                } else
                    result /= value;
                break;
            case 'h':
            case 'H':
                std::cout << "Operator   Meaning\n";
                std::cout << "   +       Add\n";
                std::cout << "   -       Subtract\n";
                std::cout << "   *       Multiply\n";
                std::cout << "   /       Divide\n";
                continue;
            default:
                std::cout << "Unknown operator " << oper_char << '\n';
                break;
        }

        std::cout << "Result: " << result << '\n';
    }
    return (0);
}
