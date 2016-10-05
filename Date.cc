using namespace std;

class Date {
    public:
        int day;
        int month;
        int year;

        Date(int d, int m, int y) {
            day = d;
            month = m;
            year = y;
        };

        string toString() {
            return to_string(month) + "/" + to_string(day) + "/" + to_string(year);
        };
};
