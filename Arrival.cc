#ifndef _Arrival_cc
#define _Arrival_cc

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
         * Constructs an Arrival object.
         *
         * @param t The type of the arrival.
         * @param d The date related to the arrival.
         * @param f The food type related to the arrival.
         * @param a The amount related to the arrival.
         */
        Arrival(string t, Date *d, string f, int a) {
            type = t;
            date = d;
            foodType = f;
            amount = a;
        };

        string getType() { return type; };
        Date* getDate() { return date; };
        string getFoodType() { return foodType; };
        int getAmount() { return amount; };
};

#endif
