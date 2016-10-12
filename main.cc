#include <iostream>
#include <string>
#include "Arrival.cc"
#include "Date.cc"

using namespace std;

Arrival* getArrival();

int main(int argc, char *argv[]) {
    Arrival* arrival1 = getArrival();
    Arrival* arrival2 = getArrival();
    cout << arrival1->date->toString() << endl;
    bool i = (*(arrival1->date)) < (*(arrival2->date));
    cout << i << endl;
    cout << arrival2->date->toString() << endl;
    //cout << arrival->toString() << "\n";
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
