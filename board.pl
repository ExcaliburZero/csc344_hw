:- module(board, [
        createBoard/4, printBoard/1
    ]).

createBoard(number(Height), number(Width), number(Mines), Board) :-
  Board = [
        ['.', '.', 'X'],
        ['.', 'X', '.'],
        ['X', '.', '.']
    ]
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
  write(' '),
  printSingleRow(Tail)
  .
