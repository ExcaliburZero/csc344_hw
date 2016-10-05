CC = g++
CFLAGS = -Wall -std=c++11
EXEC = main
OBJ = main.o

compile:
	$(CC) $(CFLAGS) -o main main.cc

clean:
	rm -rf $(EXEC) $(OBJ)
