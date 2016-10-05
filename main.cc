#include <iostream>
#include <string>
#include "Arrival.cc"

using namespace std;

Arrival* getArrival();
Date* parseDate(string);

int main(int argc, char *argv[]) {
    Arrival* arrival = getArrival();
    cout << arrival->toString() << "\n";
    return 0;
}

/**
 * Takes in an arrival from the user and returns the generated Arrival object.
 *
 * @returns The user created Arrival object.
 */
Arrival* getArrival() {
    string type;
    string dateString;
    string foodType;
    int amount;

    cin >> type >> dateString >> foodType >> amount;

    Date *date = parseDate(dateString);

    return new Arrival(type, date, foodType, amount);
}

/**
 * Converts the given string representation of a date into a Date object.
 *
 * @param dateString The string to be converted into a Date.
 * @returns The converted Date object.
 */
Date* parseDate(string dateString) {
    int day = stoi(dateString.substr(3, 5));
    int month = stoi(dateString.substr(0, 2));
    int year = stoi(dateString.substr(6, 10));

    return new Date(day, month, year);
}
