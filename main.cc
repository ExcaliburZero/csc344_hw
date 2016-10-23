/*
 * Student Name:        Christopher Wells
 * Assignment Number:   2
 * Due Date:            Nov. 2, 2016
 *
 * Program Description:
 *   Processes the buying and stocking of seafood for a seafood sales truck.
 *   Takes in arrivals via stdin and outputs the arrival information and state
 *   of the truck after each buy or stock arrival.
 *
 *   The program halts after the end of file (EOF) is reached. To simulate this
 *   when giving input through the terminal you can press Ctrl+D on Unix
 *   systems.
 *
 * Implementation Description:
 *   The program breaks down the problem into Arrival events, Trucks, and
 *   Boxes. The Truck uses priority queues to store the different types of
 *   boxes taking into account if they are open, and their expiration dates.
 *
 *   Each arrival event entered is parsed into an Arrival object, which can
 *   represent either a buy or stock event.
 *
 *   The arrival is then sent to the Truck, which acts based on the arrival.
 *
 *   If it is a stock event, then the specified amount of boxes of the given
 *   type and expiration date are added to the corresponding Box queue.
 *
 *   If it is a buy event, then ...
 *
 * Input:
 *   ./main < INPUT_FILE
 *
 *   Where INPUT_FILE is a text file containing arrival instances in the
 *   following form:
 *
 *   ARRIVAL DATE FOOD_TYPE AMOUNT
 *
 *   Ex.
 *     stock 10/12/2014 crab 4
 *     stock 12/28/2012 shrimp 2
 *     buy 02/01/2016 crab 1
 *
 * Output:
 *   first_arrival
 *   new_state_of_truck
 *   second_arrival
 *   new_state_of_truck
 *   ...
 *   last_arrival
 *   final_state_of_truck
 *
 * Errors:
 *   - If no input is given other than just an EOF, the program ends without
 *     giving any output.
 */
#include <iostream>
#include <string>
#include "Arrival.h"
#include "Date.h"
#include "Truck.h"

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
            cout << truck << endl;
        } else {
            running = false;
        }
    }

    return 0;
}
