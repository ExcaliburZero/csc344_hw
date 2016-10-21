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
    if (isOpen) {
        if (o.isOpen) {
            return *expirationDate < *o.expirationDate;
        } else {
            return 0;
        }
    } else if (o.isOpen) {
        return 1;
    } else {
        return *expirationDate < *o.expirationDate;
    }
};

string Box::toString() {
    return "[" + to_string(itemsLeft) + "]";
}

ostream& operator<<(ostream &output, const Box &box) {
    output << "[" << to_string(box.itemsLeft) << "]";
    return output;
}
