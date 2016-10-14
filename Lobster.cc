#ifndef _Lobster_cc
#define _Lobster_cc

#include "Box.cc"

class Lobster : public Box {
    public:
        Lobster(Date *date) {
            isOpen = false;
            maxItems = 4;
            itemsLeft = maxItems;
            expirationDate = date;
        };
};

#endif
