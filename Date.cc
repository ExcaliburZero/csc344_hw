#ifndef _Date_cc
#define _Date_cc

#include <iostream>

using namespace std;

class Date {
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
        Date(string dateString) {
            day = stoi(dateString.substr(3, 5));
            month = stoi(dateString.substr(0, 2));
            year = stoi(dateString.substr(6, 10));
        };

        string toString() {
            return to_string(month) + "/" + to_string(day) + "/" + to_string(year);
        };

        /**
         * Checks if this Date is less than the given other Date. Based on year, then month, then day.
         *
         * @param o The other Date.
         * @returns If this Date is less than the other Date.
         */
        bool operator < (const Date& o) const {
            if (year != o.year) {
                return year < o.year;
            } else if (month != o.month) {
                return month < o.month;
            } else if (day != o.day) {
                return day < o.day;
            } else {
                return false;
            }
        }
};

#endif
