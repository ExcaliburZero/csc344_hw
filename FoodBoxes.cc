#include "Box.h"
#include "FoodBoxes.h"

Shrimp::Shrimp(Date *date) {
    isOpen = false;
    maxItems = 50;
    itemsLeft = maxItems;
    expirationDate = date;
};

Lobster::Lobster(Date *date) {
    isOpen = false;
    maxItems = 4;
    itemsLeft = maxItems;
    expirationDate = date;
};

Crab::Crab(Date *date) {
    isOpen = false;
    maxItems = 6;
    itemsLeft = maxItems;
    expirationDate = date;
};

Swordfish::Swordfish(Date *date) {
    isOpen = false;
    maxItems = 8;
    itemsLeft = maxItems;
    expirationDate = date;
};
