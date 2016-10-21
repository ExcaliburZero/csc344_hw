CC = g++
CFLAGS = -Wall -std=c++11
OBJS = main.o Arrival.o Date.o Box.o FoodBoxes.o Truck.o
HDRS = Arrival.h Date.h Box.h FoodBoxes.h Truck.h
EXEC = main

.SUFFIXES: .cc
.cc.o:
	$(CC) -c $(CFLAGS) $*.cc

default: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(CFLAGS) -o $(EXEC) $(OBJS)

$(OBJS): $(HDRS)

clean:
	rm -rf $(EXEC) $(OBJS)
