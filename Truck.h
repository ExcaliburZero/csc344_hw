#ifndef _Truck_h
#define _Truck_h

#include <queue>
#include <ostream>
#include <vector>
#include "Box.h"
#include "FoodBoxes.h"

using namespace std;

class Truck {
    friend string boxesToString (priority_queue <Box>);
    friend ostream &operator<<(ostream &output, const Truck &truck);

    private:
        priority_queue <Box> shrimp;
        priority_queue <Box> lobster;
        priority_queue <Box> crab;
        priority_queue <Box> swordfish;

        vector <Arrival*> queuedBuys;

        /**
         * Gets the queue for the given food type.
         *
         * @param foodType The food type to return the queue for.
         * @returns The queue of Boxes of the given foodType.
         */
        priority_queue <Box>* getQueue(string foodType);

        /**
         * Checks if the Truck has queued buys for the given food type.
         *
         * @param foodType The food type to check for queued buys of.
         * @returns Whether or not the Truck has queued buys for the given food
         * type.
         */
        bool hasQueuedBuys(string foodType);


        /**
         * Attempts to process the queued buys of the given food type.
         *
         * @param foodType The food type to process the queued buys of.
         */
        void processQueuedBuys(string foodType);

        /**
         * Adds a box of the given food type with the given expiration date to
         * the Truck.
         *
         * @param foodType The type of box to add.
         * @param date The expiration date of the box to add.
         */
        void addBox(string foodType, Date *date);

        /**
         * Removes the given amount of food items from the given queue of
         * Boxes.
         *
         * @param queue The queue to get the boxes of food items from.
         * @param amount The amount of food items to remove.
         */
        void removeItems(priority_queue <Box> *queue, int amount);

        /**
         * Checks is the Truck can fill the given buy order.
         *
         * @param buy The buy order to check.
         * @returns Whether the Truck can fill the given buy order or not.
         */
        bool canFillBuy(Arrival *buy);

        /**
         * Processes the given stock Arrival.
         *
         * @param arrival The stock Arrival to be processed.
         */
        void processStock(Arrival *arrival);

        /**
         * Processes the given buy Arrival.
         *
         * @param arrival The buy Arrival to be processed.
         */
        void processBuy(Arrival *arrival);

    public:
        ~Truck();

        /**
         * Processes the given Arrival.
         *
         * @param arrival The Arrival to be processed.
         */
        void processArrival(Arrival *arrival);
};

#endif
