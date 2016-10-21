#ifndef _Box_h
#define _Box_h

#include "Date.h"

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
        int removeItems(int itemsNumber);

        /**
         * Checks if the Box is less than another Box object. Based on whether
         * the Boxes are open, and their expiration Date.
         *
         * @param o The other Box.
         * @returns If this Box is less than the other Box.
         */
        bool operator < (const Box& o) const;

        string toString();

        friend ostream &operator<<(ostream &output, const Box &box);
};

#endif
