#include <iostream>
#include <string>
#include "Arrival.cc"

using namespace std;

Arrival* getArrival();

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

    Date *date = new Date(dateString);

    return new Arrival(type, date, foodType, amount);
}
