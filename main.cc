#include <iostream>
#include <string>
#include "Arrival.cc"
#include "Date.cc"
#include "Truck.cc"

using namespace std;

Arrival* getArrival();

int main(int argc, char *argv[]) {
    Truck truck;
    truck.addBox("swordfish", new Date("10/15/2016"));
    truck.addBox("swordfish", new Date("10/15/2016"));
    cout << truck.swordfish.top().itemsLeft << endl;
    truck.processArrival(new Arrival("buy", new Date("10/15/2016"), "swordfish", 18));
    cout << truck.swordfish.top().itemsLeft << endl;
    cout << "Size: " << truck.swordfish.size() << endl;
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
