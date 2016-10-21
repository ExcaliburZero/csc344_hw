#include <queue>
#include <ostream>
#include "Arrival.h"
#include "Box.h"
#include "Truck.h"
#include "FoodBoxes.h"

using namespace std;

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

void Truck::processStock(Arrival *arrival) {
    string foodType = arrival->getFoodType();
    if (foodType == "shrimp"    ||
        foodType == "lobster"   ||
        foodType == "crab"      ||
        foodType == "swordfish") {
        for (int i = 0; i < arrival->getAmount(); i++) {
            addBox(foodType, arrival->getDate());
        }
    } else {
        cout << "Invalid food type: " << foodType << endl;
    }
};

void Truck::processBuy(Arrival *arrival) {
    string foodType = arrival->getFoodType();
    int amount = arrival->getAmount();
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

void Truck::processArrival(Arrival *arrival) {
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
    output << "\n\n\n";
    return output;
};
