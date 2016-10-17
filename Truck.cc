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

        void removeItems(priority_queue <Box> *queue, int amount) {
            int remainingAmount = amount;
            while (remainingAmount != 0 && queue->size() > 0) {
                Box topBox = queue->top();
                queue->pop();
                remainingAmount = topBox.removeItems(remainingAmount);

                if (topBox.itemsLeft > 0) {
                    queue->push(topBox);
                }
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
            string foodType = arrival->foodType;
            int amount = arrival->amount;
            if (foodType == "shrimp") {
                removeItems(&shrimp, amount);
            } else if (foodType == "lobster") {
                removeItems(&lobster, amount);
            } else if (foodType == "crab") {
                removeItems(&crab, amount);
            } else if (foodType == "swordfish") {
                removeItems(&swordfish, amount);
            } else {
                cout << "Invalid food type: " << foodType << endl;
            }
        };
};

#endif
