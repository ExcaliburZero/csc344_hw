createBoard(number(Height), number(Width), [
        ['.', '.', 'X'],
        ['X', '.', '.']
    ]) :-
  write('('),
  write(Height), write(', '),
  write(Width), write(')'), nl
  .

printBoard(Board) :-
  printRows(Board)
  .

printRows([]).

printRows([Head|Tail]) :-
  printSingleRow(Head),
  nl,
  printRows(Tail)
  .

printSingleRow([]).

printSingleRow([Head|Tail]) :-
  write(Head),
  printSingleRow(Tail)
  .
