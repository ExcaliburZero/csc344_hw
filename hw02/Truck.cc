#include <queue>
#include <ostream>
#include "Arrival.h"
#include "Box.h"
#include "Truck.h"
#include "FoodBoxes.h"

using namespace std;

Truck::~Truck() {
    int numberOfBuys = queuedBuys.size();
    
    for (int i = 0; i < numberOfBuys; i++) {
        delete queuedBuys[i];
    }
};

priority_queue <Box>* Truck::getQueue(string foodType) {
    if (foodType == "shrimp") {
        return &shrimp;
    } else if (foodType == "lobster") {
        return &lobster;
    } else if (foodType == "crab") {
        return &crab;
    } else if (foodType == "swordfish") {
        return &swordfish;
    } else {
        return NULL;
    }
};

void Truck::addBox(string foodType, Date *date) {
    if (foodType == "shrimp") {
        shrimp.push(Shrimp(date));
    } else if (foodType == "lobster") {
        lobster.push(Lobster(date));
    } else if (foodType == "crab") {
        crab.push(Crab(date));
    } else if (foodType == "swordfish") {
        swordfish.push(Swordfish(date));
    }
};

void Truck::removeItems(priority_queue <Box> *queue, int amount) {
    int remainingAmount = amount;
    while (remainingAmount != 0 && queue->size() > 0) {
        Box topBox = queue->top();
        queue->pop();
        remainingAmount = topBox.removeItems(remainingAmount);

        if (topBox.itemsLeft > 0) {
            queue->push(topBox);
        }
    }
};

bool Truck::canFillBuy(Arrival *buy) {
    string foodType = buy->getFoodType();
    int amount = buy->getAmount();

    // Calculate the amount of food of the given type that can be sold
    int foodAvailable = 0;
    priority_queue <Box>* foodQueue = getQueue(foodType);
    if (foodQueue->size() > 0) {
        Box topBox = foodQueue->top();

        foodAvailable += topBox.itemsLeft;
        foodAvailable += (foodQueue->size() - 1) * topBox.maxItems;
    }

    return amount <= foodAvailable;
};

void Truck::processQueuedBuys(string foodType) {
    int numberOfBuys = queuedBuys.size();

    for (int i = 0; i < numberOfBuys; i++) {
        Arrival* currentBuy = queuedBuys.at(i);
        if (currentBuy->getFoodType() == foodType) {
            // Process the queued buy, it will be re-added to the queue if it
            // cannot be filled
            queuedBuys.erase(queuedBuys.begin() + i);
            processBuy(currentBuy);
        }
    }
};

void Truck::processStock(Arrival *arrival) {
    string foodType = arrival->getFoodType();

    // Stock the new boxes
    if (foodType == "shrimp"    ||
        foodType == "lobster"   ||
        foodType == "crab"      ||
        foodType == "swordfish") {
        for (int i = 0; i < arrival->getAmount(); i++) {
            addBox(foodType, arrival->getDate());
        }
        cout << "Stocked arrival:  " << *arrival << endl;
        delete arrival;
    } else {
        cout << "Invalid food type: " << foodType << endl;
    }

    // Attempt to process any queued buys
    processQueuedBuys(foodType);
};

void Truck::processBuy(Arrival *arrival) {
    string foodType = arrival->getFoodType();
    if (canFillBuy(arrival)) {
        // If the buy can be filled, then fill it
        int amount = arrival->getAmount();
        if (foodType == "shrimp"    ||
            foodType == "lobster"   ||
            foodType == "crab"      ||
            foodType == "swordfish") {
            removeItems(getQueue(foodType), amount);
            cout << "Filled buy:       " << *arrival << endl;
            delete arrival;
        } else {
            cout << "Invalid food type: " << foodType << endl;
        }
    } else {
        // If the buy cannot be filled, then queue it for later
        queuedBuys.push_back(arrival);
    }
};

void Truck::processArrival(Arrival *arrival) {
    cout << "Recieved arrival: " << *arrival << endl;

    string arrivalType = arrival->getType();
    if (arrivalType == "stock") {
        processStock(arrival);
    } else if (arrivalType == "buy") {
        processBuy(arrival);
    } else {
        cout << "Invalid arrival type: " << arrivalType << endl;
    }
};

string boxesToString (priority_queue <Box> boxes) {
    string boxString = "";
    priority_queue <Box> boxesCopy(boxes);
    int numberOfBoxes = boxesCopy.size();
    for (int i = 0; i < numberOfBoxes; i++) {
        Box box = boxesCopy.top();
        boxString += box.toString();
        boxesCopy.pop();
    }
    return boxString;
};

ostream &operator<<(ostream &output, const Truck &truck) {
    string divider = "---------------\n";
    string dividerSmall = "------\n";
    output << divider << "Truck\n" << divider;
    output << "Shrimp:    " + to_string(truck.shrimp.size()) + "\n";
    output << boxesToString(truck.shrimp) << "\n";
    output << dividerSmall;
    output << "Lobster:   " + to_string(truck.lobster.size()) + "\n";
    output << boxesToString(truck.lobster) << "\n";
    output << dividerSmall;
    output << "Crab:      " + to_string(truck.crab.size()) + "\n";
    output << boxesToString(truck.crab) << "\n";
    output << dividerSmall;
    output << "Swordfish: " + to_string(truck.swordfish.size()) + "\n";
    output << boxesToString(truck.swordfish) << "\n";
    output << dividerSmall;
    output << "Queued Buys: " + to_string(truck.queuedBuys.size()) + "\n";
    output << dividerSmall;
    output << "\n\n\n";
    return output;
};
