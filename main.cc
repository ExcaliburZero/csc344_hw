#include <iostream>
#include <string>
#include "Arrival.cc"
#include "Date.cc"
#include "Truck.cc"

using namespace std;

int main(int argc, char *argv[]) {
    Truck truck;

    // Continually get input in until the end of fle is reached.
    bool running = true;
    while (running) {
        string line;
        getline(cin, line);
        int eof = 0;
        if (line.front() != eof) {
            Arrival *arrival = new Arrival(line);
            truck.processArrival(arrival);
            delete arrival;
            cout << truck << endl;
        } else {
            running = false;
        }
    }

    return 0;
}
