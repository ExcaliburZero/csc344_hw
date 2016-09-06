/*
 * Student Name:        Christopher Wells
 * Assignment Number:   1.B
 * Due Date:            XXXXXXXX
 *
 * Program Description:
 *   Prints out a list of random numbers. By default 10 random numbers between
 *   1 and 100 inclusive are printed, however the amount of numbers and the
 *   range can be changed through the use of command line flags.
 *
 * Implementation Description:
 *   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 *
 * Input:
 *   ./randoms [OPTIONS]
 *
 *   -n COUNT          Specifies the number of random numbers to generate.
 *   -l LOWER_BOUND    Specifies the lower bound of the random number range.
 *   -u UPPER_BOUND    Specifies the upper bound of the random number range.
 *
 * Output:
 *   first_random_number
 *   second_random_number
 *   ...
 *   last_random_number
 *
 * Errors:
 *   [ ] If the user gives a count zero or less then an error message is printed.
 *   [ ] If the user gives an invalid range where the upper bound is below the lower bound then an error message is printed.
 *   [ ] If the user gives one or more invalid flags then an error message is printed.
 *
 * Data Description:
 *   XXXXXXXXXXXXXXXXXXXXXXXXXXXX
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/**
 * Returns an array of random numbers of the given amount within the given
 * upper and lower bounds.
 *
 * @param amount The amount of random numbers to return.
 * @param lowerBound The lower bound of the random numbers.
 * @param upperBound The upper bound of the random numbers.
 * @returns An array of random numbers.
 *
 * >>> generateRandomNumbers(4, 1, 100)
 * [55, 90, 1, 100]
 * >>> generateRandomNumbers(4, 1, 100)
 * [45, 63, 2, 25]
 */
int *generateRandomNumbers(int amount, int lowerBound, int upperBound) {
    int *randomNumbers = malloc(sizeof(int) * amount);
    return randomNumbers;
}

/**
 *
 */
int main(int argc, char *argv[]) {
    int amount = 10;
    int lowerBound = 1;
    int upperBound = 100;
    int *randomNumbers = generateRandomNumbers(amount, lowerBound, upperBound);

    for (int i = 0; i < amount; i++) {
        printf("%d\n", randomNumbers[i]);
    }
}
