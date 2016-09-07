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
 *   Random number generation is handled using the rand() function seeded using
 *   time(NULL) as shown in the following Stack Overflow answer.
 *
 *   http://stackoverflow.com/a/822368/4764550
 *
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
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

int getRandomNum(int, int);
int *generateRandomNumbers(int, int, int);

/**
 *
 */
int main(int argc, char *argv[]) {
    // Set the default values
    int amount = 10;
    int lowerBound = 1;
    int upperBound = 100;

    // Handle the command line arguments
    char *options = ":n:l:u:";
    int c;
    while ((c = getopt(argc, argv, options)) != -1) {
        switch (c) {
            case 'n':
                amount = atoi(optarg);
                break;
            case 'l':
                lowerBound = atoi(optarg);
                break;
            case 'u':
                upperBound = atoi(optarg);
                break;
        }
    }

    // Generate and print the random numbers
    int *randomNumbers = generateRandomNumbers(amount, lowerBound, upperBound);
    for (int i = 0; i < amount; i++) {
        printf("%d\n", randomNumbers[i]);
    }
}

/**
 * Returns a random integer within the given range inclusively.
 *
 * @param lowerBound The lower bound of the random numbers.
 * @param upperBound The upper bound of the random numbers.
 * @returns A random number within the given range.
 *
 * >>> getRandomNum(1, 10)
 * 5
 * >>> getRandomNum(1, 10)
 * 1
 */
int getRandomNum(int lowerBound, int upperBound) {
    int difference = upperBound - lowerBound + 1;
    return (rand() % difference) + lowerBound;
}

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

    // Seed the random number generator
    srand(time(NULL));

    // Generate random numbers and add them to the array until it is filled
    for (int i = 0; i < amount; i++) {
        randomNumbers[i] = getRandomNum(lowerBound, upperBound);
    }

    return randomNumbers;
}
