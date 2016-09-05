CC = gcc
CFLAGS = -Wall
OBJS = permutations.o
EXEC = permutations

permutations:
	$(CC) $(CFLAGS) -o permutations permutations.c

clean:
	rm -f $(OBJS) $(EXEC)
