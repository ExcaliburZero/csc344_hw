#ifndef _FoodBoxes_h
#define _FoodBoxes_h

#include "Box.h"
#include "Date.h"

class Shrimp : public Box {
    public:
        Shrimp(Date *date);
};

class Lobster : public Box {
    public:
        Lobster(Date *date);
};

class Crab : public Box {
    public:
        Crab(Date *date);
};

class Swordfish : public Box {
    public:
        Swordfish(Date *date);
};

#endif
