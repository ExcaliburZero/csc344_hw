#ifndef _Date_h
#define _Date_h

#include <iostream>

using namespace std;

class Date {
    friend ostream &operator<<(ostream &output, const Date &date);

    private:
        int day;
        int month;
        int year;

    public:
        /**
         * Constructs a Date from a date string in the format MM/DD/YYYY.
         *
         * Ex. "10/01/2015"
         */
        Date(string dateString);

        /**
         * Checks if this Date is less than the given other Date. Based on year, then month, then day.
         *
         * @param o The other Date.
         * @returns If this Date is less than the other Date.
         */
        bool operator < (const Date& o) const;
};

#endif
