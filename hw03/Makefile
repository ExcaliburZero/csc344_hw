EXEC = minesweeper.pl
TESTS = win lose explore

play:
	swipl $(EXEC)

$(TESTS):
	swipl $(EXEC) < data/$@.txt > data/$@.out
	cmp data/$@.case data/$@.out

test: $(TESTS)

clean:
	rm data/*.out
