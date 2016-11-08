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
  readCommand(Type, Xp, Yp),
  atom_number(Xp, X),
  atom_number(Yp, Y),
  (
      Type = 'Press' ->
      pressPoint(MineBoard, VisualBoard, X, Y, NewVisualBoard)
  ;
      Type = 'Flag' ->
      flagPoint(MineBoard, VisualBoard, X, Y, NewVisualBoard)
  ),
  printBoard(NewVisualBoard),
  promptPress(MineBoard, NewVisualBoard)
  .
