#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Returns the factorial of the given number.
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
 * >>> getPermutations("hi")
 * ["hi", "ih"]
 */
char **getPermutations(char *word) {
    int wordLength = strlen(word);
    int numberOfPermutations = factorial(wordLength);
    char **permutations = malloc(sizeof(char) * numberOfPermutations);
    permutations[0] = "hi";
    permutations[1] = "ih";
    return permutations;
}

/**
 * Prints out all of the permutations of the string given in through stdin.
 */
int main(int argc, char *argv[]) {
    char *word = "Hi";
    char **permutations = getPermutations(word);
    int numberOfPermutations = strlen(word);
    for (int i = 0; i < numberOfPermutations; i++) {
        printf("%s\n", permutations[i]);
    }
    free(permutations);
    printf("hello!");
}
