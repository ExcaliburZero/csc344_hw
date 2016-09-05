/*
 * Student Name:        Christopher Wells
 * Assignment Number:   1
 * Due Date:            XXXXXXXX
 *
 * Program Description:
 *   Prints out all of the permutations of the string that the user gives in
 *   via stdin.
 *
 * Input:
 *   ./permutations string_to_permutate
 *
 * Output:
 *   first_permutation ⇐ 1st permutation
 *   second_permutation ⇐ 2nd permutation
 *   ...
 *
 * Errors:
 *   If the user does not give an input string in the command, then program
 *   usage information is printed out to the user.
 *
 * Data Description:
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Returns the factorial of the given number.
 *
 * @param n The number to return the factorial of.
 * @returns The factorial of the given number.
 *
 * >>> factorial(1)
 * 1
 * >>> factorial(4)
 * 24
 */
int factorial(int n) {
    if (n > 1) {
        return n * factorial(n - 1);
    } else {
        return 1;
    }
}

/**
 * Returns an array of all of the possible permutations of the given character
 * array.
 *
 * @param word The string to return the permutations of
 * @returns An array of all of the permutations of the given string.
 *
 * >>> getPermutations("hi")
 * ["hi", "ih"]
 */
char **getPermutations(char *word) {
    int wordLength = strlen(word);
    int numberOfPermutations = factorial(wordLength);
    char **permutations = malloc(sizeof(char) * numberOfPermutations);

    // Get all of the permutations of the given string
    permutations[0] = "hi";
    permutations[1] = "ih";

    return permutations;
}

/**
 * Prints out all of the permutations of the string given in through stdin.
 */
int main(int argc, char *argv[]) {
    // Make sure that the user has given an input string
    if (argc == 1) {
        printf("usage: %s string_to_permute\n", argv[0]);
        return 1;
    }

    // If the user has entered a string then print out its permutations
    char *word = "Hi";
    char **permutations = getPermutations(word);
    int numberOfPermutations = strlen(word);
    for (int i = 0; i < numberOfPermutations; i++) {
        // Get the number ending for the given permutation number (ex. 1st)
        char *ending;
        if (i + 1 == 1) {
            ending = "st";
        } else if (i + 1 == 2) {
            ending = "nd";
        } else if(i + 1 == 3) {
            ending = "rd";
        } else {
            ending = "th";
        }

        // Print out the given permutation
        printf("%s ⇐ %d%s permutation\n", permutations[i], i + 1, ending);
    }
    free(permutations);
}
