/*
 * Student Name:        Christopher Wells
 * Assignment Number:   1.B
 * Due Date:            Sep. 23, 2016
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
 *   getopt() is used to parse the command line arguments. atoi() is used to
 *   convert the string aruments into int values.
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
 *   - If the user gives a count of zero or less then an error message is
 *     printed.
 *
 *   - If the user gives an invalid range where the upper bound is below the
 *     lower bound then an error message is printed.
 *
 *   - If the user gives one or more invalid flags then an error message is
 *     printed.
 *
 *   - If the user does not give a flag argument then an error message is
 *     printed.
 */
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

int getRandomNum(int, int);
int *generateRandomNumbers(int, int, int);

/**
 * Prints out random numbers in an amount and range specified through command
 * line flags.
 *
 * @param argc The amount of command line arguments.
 * @param argv The command line arguments.
 * @returns The return code of the program.
 */
int main(int argc, char *argv[]) {
    int invalidArgumentError = 22;

    // Set the default values
    int amount = 10;
    int lowerBound = 1;
    int upperBound = 100;

    // Handle the command line flags
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
            case '?':
                fprintf(stderr, "Invalid flag -%c.\n", optopt);
                return invalidArgumentError;
                break;
            case ':':
                fprintf(stderr, "Arguement missing for -%c flag.\n", optopt);
                return invalidArgumentError;
                break;
        }
    }

    // Check for invalid settings
    if (amount <= 0) {
        fprintf(stderr, "Invalid amount %d.\nAmount must be greater than zero.\n", amount);
        return invalidArgumentError;
    }
    if (lowerBound > upperBound) {
        fprintf(stderr, "Invalid range %d-%d.\nLower bound must be lesser than the upper bound.\n", lowerBound, upperBound);
        return invalidArgumentError;
    }

    // Generate and print the random numbers
    int *randomNumbers = generateRandomNumbers(amount, lowerBound, upperBound);
    for (int i = 0; i < amount; i++) {
        printf("%d\n", randomNumbers[i]);
    }
    free(randomNumbers);
    return 0;
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
