#ifndef _Truck_cc
#define _Truck_cc

#include <queue>
#include "Crab.cc"
#include "Lobster.cc"
#include "Shrimp.cc"
#include "Swordfish.cc"

class Truck {
    public:
        priority_queue <Box> shrimp;
        priority_queue <Box> lobster;
        priority_queue <Box> crab;
        priority_queue <Box> swordfish;

        void addBox(string foodType, Date *date) {
            if (foodType == "shrimp") {
                shrimp.push(Shrimp(date));
            } else if (foodType == "lobster") {
                lobster.push(Lobster(date));
            } else if (foodType == "crab") {
                crab.push(Crab(date));
            } else if (foodType == "swordfish") {
                swordfish.push(Swordfish(date));
            }
        }

        void processArrival(const Arrival *arrival) {
            if (arrival->type == "stock") {
                processStock(arrival);
            } else if (arrival->type == "buy") {
                processBuy(arrival);
            } else {
                cout << "Invalid arrival type: " << arrival->type << endl;
            }
        };

        void processStock(const Arrival *arrival) {
            string foodType = arrival->foodType;
            if (foodType == "shrimp"    ||
                foodType == "lobster"   ||
                foodType == "crab"      ||
                foodType == "swordfish") {
                for (int i = 0; i < arrival->amount; i++) {
                    addBox(foodType, arrival->date);
                }
            } else {
                cout << "Invalid food type: " << foodType << endl;
            }
        };

        void processBuy(const Arrival *arrival) {
            cout << "buy" << endl;
        };
};

#endif
