using namespace std;

class Arrival {
    public:
        string type;
        string date;
        string foodType;
        int amount;

        Arrival(string t, string d, string f, int a) {
            type = t;
            date = d;
            foodType = f;
            amount = a;
        };
};
