#ifndef _Shrimp_cc
#define _Shrimp_cc

#include "Box.cc"

class Shrimp : public Box {
    public:
        Shrimp(Date *date) {
            isOpen = false;
            maxItems = 50;
            itemsLeft = maxItems;
            expirationDate = date;
        };
};

#endif
