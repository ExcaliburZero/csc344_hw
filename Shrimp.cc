#ifndef _Shrimp_cc
#define _Shrimp_cc

class Shrimp : public Box {
    public:
        Shrimp(string date) {
            isOpen = false;
            maxItems = 50;
            itemsLeft = maxItems;
            expirationDate = date;
        };
};

#endif
