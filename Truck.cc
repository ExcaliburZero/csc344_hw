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

        /**
         * Adds a box of the given food type with the given expiration date to
         * the Truck.
         *
         * @param foodType The type of box to add.
         * @param date The expiration date of the box to add.
         */
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

        /**
         * Removes the given amount of food items from the given queue of
         * Boxes.
         *
         * @param queue The queue to get the boxes of food items from.
         * @param amount The amount of food items to remove.
         */
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

        /**
         * Processes the given Arrival.
         *
         * @param arrival The Arrival to be processed.
         */
        void processArrival(const Arrival *arrival) {
            if (arrival->type == "stock") {
                processStock(arrival);
            } else if (arrival->type == "buy") {
                processBuy(arrival);
            } else {
                cout << "Invalid arrival type: " << arrival->type << endl;
            }
        };

        /**
         * Processes the given stock Arrival.
         *
         * @param arrival The stock Arrival to be processed.
         */
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

        /**
         * Processes the given buy Arrival.
         *
         * @param arrival The buy Arrival to be processed.
         */
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
