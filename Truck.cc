#ifndef _Truck_cc
#define _Truck_cc

#include <queue>
#include "Lobster.cc"
#include "Shrimp.cc"

class Truck {
    public:
        priority_queue <Box> shrimp;
        priority_queue <Box> lobster;

        void addBox(string foodType, Date *date) {
            if (foodType == "shrimp") {
                shrimp.push(Shrimp(date));
            } else if (foodType == "lobster") {
                lobster.push(Lobster(date));
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
            if (arrival->foodType == "shrimp") {
                for (int i = 0; i < arrival->amount; i++) {
                    addBox("shrimp", arrival->date);
                }
            } else if (arrival->foodType == "lobster") {
                for (int i = 0; i < arrival->amount; i++) {
                    addBox("lobster", arrival->date);
                }
            } else {
                cout << "Invalid food type: " << arrival->foodType << endl;
            }
        };

        void processBuy(const Arrival *arrival) {
            cout << "buy" << endl;
        };
};

#endif
