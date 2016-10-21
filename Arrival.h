#ifndef _Arrival_h
#define _Arrival_h

#include "Date.h"

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
        Arrival(string arrivalString);

        ~Arrival();

        string getType();
        Date* getDate();
        string getFoodType();
        int getAmount();
};

#endif
