#include <iostream>
#include <sstream>
#include "Date.h"

using namespace std;

Date::Date(string dateString) {
    string dStrings[3];
    char delimiter = '/';

    istringstream dateStream(dateString);
    string token;
    int i = 0;
    while (i < 3 && getline(dateStream, token, delimiter)) {
        dStrings[i] = token;
        i++;
    }

    month = stoi(dStrings[0]);
    day = stoi(dStrings[1]);
    year = stoi(dStrings[2]);
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

string Date::into_string() {
    return to_string(month) + "/" + to_string(day) + "/" + to_string(year);
}

ostream &operator<<(ostream &output, const Date &date) {
    output << to_string(date.month) << "/" << to_string(date.day) << "/" << to_string(date.year);
    return output;
};
