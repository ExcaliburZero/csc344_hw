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
 *   first_permutation
 *   second_permutation
 *   ...
 *   last_permutation
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

void swap(char *list, int a, int b) {
    int holder = list[a];
    list[a] = list[b];
    list[b] = holder;
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
    char **permutations = malloc(sizeof(char) * numberOfPermutations * wordLength * 2);

    // Use Heap's Algorithm as per the psedocode here:
    // https://en.wikipedia.org/w/index.php?title=Heap%27s_algorithm&oldid=733235123
    int nextPermutation = 0;
    int counters[wordLength];
    for (int i = 0; i < wordLength; i++) {
        counters[i] = 0;
    }

    char *word2 = malloc(sizeof(char) * wordLength);
    permutations[nextPermutation++] = word;
    strcpy(word2, word);
    word = malloc(sizeof(char) * wordLength);
    strcpy(word, word2);
    int i = 0;
    while (i < wordLength) {
        if (counters[i] < i) {
            if (i % 2 == 0) {
                swap(word, 0, i);
            } else {
                swap(word, counters[i], i);
            }
            permutations[nextPermutation++] = word;
            strcpy(word2, word);
            word = malloc(sizeof(char) * wordLength);
            strcpy(word, word2);
            counters[i]++;
            i = 0;
        } else {
            counters[i] = 0;
            i++;
        }
    }

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
    char *word = argv[1];
    int wordLength = strlen(word);
    int numberOfPermutations = factorial(wordLength);
    char **permutations = getPermutations(word);
    for (int i = 0; i < numberOfPermutations; i++) {
        // Print out the given permutation
        printf("%s\n", permutations[i]);
    }
    free(permutations);
}
