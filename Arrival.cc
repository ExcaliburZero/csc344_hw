#ifndef _Arrival_cc
#define _Arrival_cc

#include <sstream>
#include "Date.cc"

using namespace std;

/**
 * An Arrival represents an input event.
 *
 * Ex: stock 10/01/2015 crab 3
 */
class Arrival {
    private:
        /**
         * The type of the arrival. Ex: stock, buy.
         */
        string type;
        /**
         * The date related to the arrival. Ex: 10/01/2015.
         */
        Date *date;
        /**
         * The food type related to the arrival. Ex: shrimp, crab.
         */
        string foodType;
        /**
         * The amount related to the arrival. Ex: 3, 10.
         */
        int amount;

    public:
        /**
         * Creates an Arrival from the given arrival string.
         *
         * The string should be formatted as type, date, food type, and amount.
         *
         * Ex: "stock 10/12/2014 crab 4"
         *
         * @param arrivalString The string representing the Arrival.
         */
        Arrival(string arrivalString) {
            string dateString;

            istringstream arrivalStream(arrivalString);
            arrivalStream >> type >> dateString >> foodType >> amount;

            date = new Date(dateString);
        };

        ~Arrival() {
            delete date;
        }

        string getType() { return type; };
        Date* getDate() { return date; };
        string getFoodType() { return foodType; };
        int getAmount() { return amount; };
};

#endif
