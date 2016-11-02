#include "Box.h"

using namespace std;

int Box::removeItems(int itemsNumber) {
    if (!isOpen) {
        isOpen = true;
    }

    if (itemsLeft >= itemsNumber) {
        itemsLeft -= itemsNumber;
        return 0;
    } else {
        int remaining = itemsNumber - itemsLeft;
        itemsLeft = 0;
        return remaining;
    }
};

bool Box::operator < (const Box& o) const {
    if (isOpen == o.isOpen) {
        return *o.expirationDate < *expirationDate;
    } else if (isOpen) {
        return false;
    } else {
        return true;
    }
};

string Box::toString() {
    return "[" + to_string(itemsLeft) + " " + expirationDate->into_string() + "]";
}

ostream& operator<<(ostream &output, const Box &box) {
    output << "[" << to_string(box.itemsLeft) << "]";
    return output;
}
