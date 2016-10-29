:- module(minesweeper, [
        play/0
    ]).

:- use_module(board).
:- initialization play.

play :-
  createBoard(number(1), number(2), number(3), Board),
  printBoard(Board),
  halt
  .
