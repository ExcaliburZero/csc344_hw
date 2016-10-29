:- module(minesweeper, [
        play/0
    ]).

:- use_module(board).
:- initialization play.

play :-
  createBoard(number(1), number(2), number(3), MineBoard, VisualBoard),
  printBoard(VisualBoard),
  promptPress(MineBoard, VisualBoard),
  halt
  .

promptPress(MineBoard, VisualBoard) :-
  write('>'), nl,
  pressPoint(MineBoard, VisualBoard, 1, 2),
  printBoard(VisualBoard)
  .
