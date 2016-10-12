#ifndef _Box_cc
#define Box_cc

using namespace std;

class Box {
    public:
        bool isOpen;
        int maxItems;
        int itemsLeft;
        Date expirationDate;

        bool operator < (const Box& o) const {
            return expirationDate < o.expirationDate;
        };
};

#endif
