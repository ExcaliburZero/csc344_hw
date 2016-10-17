#ifndef _Crab_cc
#define _Crab_cc

#include "Box.cc"

class Crab : public Box {
    public:
        Crab(Date *date) {
            isOpen = false;
            maxItems = 6;
            itemsLeft = maxItems;
            expirationDate = date;
        };
};

#endif
