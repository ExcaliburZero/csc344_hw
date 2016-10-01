CC = g++
CFLAGS = -Wall
EXEC = main
OBJ = main.o

compile:
	$(CC) $(CFLAGS) -o main main.cc

clean:
	rm -rf $(EXEC) $(OBJ)
