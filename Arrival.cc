#include <sstream>
#include "Arrival.h"
#include "Date.h"

using namespace std;

Arrival::Arrival(string arrivalString) {
    string dateString;

    istringstream arrivalStream(arrivalString);
    arrivalStream >> type >> dateString >> foodType >> amount;

    date = new Date(dateString);
};

Arrival::~Arrival() {
    delete date;
};

string Arrival::getType() { return type; };
Date* Arrival::getDate() { return date; };
string Arrival::getFoodType() { return foodType; };
int Arrival::getAmount() { return amount; };

ostream &operator<<(ostream &output, Arrival &arrival) {
    output << arrival.getType() << " " << *(arrival.getDate()) << " ";
    output << arrival.getFoodType() << " " << to_string(arrival.getAmount());
    return output;
};
