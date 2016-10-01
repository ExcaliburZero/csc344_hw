#include <iostream>

using namespace std;

int* getInitialBoxes();

int main(int argc, char *argv[]) {
    cout << "Hello, World!\n";
    int *boxes = getInitialBoxes();
    delete [] boxes;
}

int* getInitialBoxes() {
    int *boxes = new int[4];
    int nextBox = 0;
    for(int i = 0; i < 4; i++) {
        string foodType;
        int boxAmount;
        cin >> foodType >> boxAmount;
        boxes[nextBox] = boxAmount;
    }
    return boxes;
}
