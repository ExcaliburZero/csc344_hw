#include <iostream>
#include <string>
#include "Arrival.cc"
#include "Date.cc"
#include "Truck.cc"

using namespace std;

Arrival* getArrival();

int main(int argc, char *argv[]) {
    Truck truck;
    truck.addBox("shrimp", new Date("10/27/2016"));
    truck.addBox("shrimp", new Date("10/28/2016"));
    truck.addBox("shrimp", new Date("10/29/2016"));
    truck.addBox("shrimp", new Date("10/24/2016"));
    cout << "Shrimp:  " << truck.shrimp.size() << endl;
    cout << "Lobster: " << truck.lobster.size() << endl;
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
