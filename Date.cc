#include <iostream>
#include "Date.h"

using namespace std;

Date::Date(string dateString) {
    day = stoi(dateString.substr(3, 5));
    month = stoi(dateString.substr(0, 2));
    year = stoi(dateString.substr(6, 10));
};

bool Date::operator < (const Date& o) const {
    if (year != o.year) {
        return year < o.year;
    } else if (month != o.month) {
        return month < o.month;
    } else if (day != o.day) {
        return day < o.day;
    } else {
        return false;
    }
};
