CC = gcc
CFLAGS = -Wall
OBJS = permutations.o randoms.o
EXEC = permutations randoms

permutations:
	$(CC) $(CFLAGS) -o permutations permutations.c

randoms:
	$(CC) $(CFLAGS) -o randoms randoms.c

clean:
	rm -f $(OBJS) $(EXEC)
