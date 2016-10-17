#ifndef _Swordfish_cc
#define _Swordfish_cc

#include "Box.cc"

class Swordfish : public Box {
    public:
        Swordfish(Date *date) {
            isOpen = false;
            maxItems = 8;
            itemsLeft = maxItems;
            expirationDate = date;
        };
};

#endif
