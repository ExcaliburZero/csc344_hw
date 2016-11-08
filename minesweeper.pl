:- module(minesweeper, [
        play/0
    ]).

:- use_module(board).
:- use_module(grammar).
:- initialization play.

play :-
  createBoard(number(1), number(2), number(3), MineBoard, VisualBoard),
  printBoard(VisualBoard),
  promptPress(MineBoard, VisualBoard),
  halt
  .

promptPress(MineBoard, VisualBoard) :-
  %read(X), read(Y),
  readCommand(_, Xp, Yp),
  atom_number(Xp, X),
  atom_number(Yp, Y),
  pressPoint(MineBoard, VisualBoard, X, Y, NewVisualBoard),
  printBoard(NewVisualBoard),
  promptPress(MineBoard, NewVisualBoard)
  .
