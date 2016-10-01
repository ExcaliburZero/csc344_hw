#include <iostream>
#include "Arrival.cc"

using namespace std;

Arrival* getArrival();

int main(int argc, char *argv[]) {
    Arrival* arrival = getArrival();
    cout << "type: " << arrival->type << "\n";
    cout << "date: " << arrival->date << "\n";
    cout << "food: " << arrival->foodType << "\n";
    cout << "amnt: " << arrival->amount << "\n";
    return 0;
}

/**
 * Takes in an arrival from the user and returns the generated Arrival object.
 *
 * @returns The user created Arrival object.
 */
Arrival* getArrival() {
    string type;
    string date;
    string foodType;
    int amount;

    cin >> type >> date >> foodType >> amount;

    return new Arrival(type, date, foodType, amount);
}
