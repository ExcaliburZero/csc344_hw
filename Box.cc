#ifndef _Box_cc
#define _Box_cc

using namespace std;

class Box {
    public:
        bool isOpen;
        int maxItems;
        int itemsLeft;
        Date *expirationDate;

        /**
         * Removes the given number of items from the Box. Also opens the Box
         * if it has not yet been opened.
         *
         * @param itemsNumber The number of items to remove from the Box.
         * @returns The number of items that could not be removed.
         */
        int removeItems(int itemsNumber) {
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

        bool operator < (const Box& o) const {
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
};

#endif
