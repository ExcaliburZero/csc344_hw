:- module(board, [
        createBoard/5, printBoard/1, pressPoint/4

        , checkForMine/4
    ]).

createBoard(number(Height), number(Width), number(Mines), MineBoard, VisualBoard) :-
  MineBoard = [
        [".", ".", "X"],
        [".", "X", "."],
        ["X", ".", "."]
    ],
  VisualBoard = [
        ["#", "#", "#"],
        ["#", "#", "#"],
        ["#", "#", "#"]
    ]
  .

% Board Printing

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

% Point Pressing

pressPoint(MineBoard, VisualBoard, X, Y) :-
  write('pressed':X:Y), nl,
  checkForMine(MineBoard, X, Y, IsMine)
  .

% checkForMine(+MineBoard,
%              +X, +Y, -IsMine)
%
% Checks if the given position on the given board is a mine or not. Sets IsMine
% to 0 if it is a mine, or a non-zero value if it is not a mine.
checkForMine(MineBoard, X, Y, IsMine) :-
  getElem(MineBoard, X, Row),
  getElem(Row, Y, Element),
  IsMine is Element - "X",
  write(IsMine), nl
  .

% getElem(+List,
%         +Index, -Element)
%
% Gets the element in the given list at the given index.
getElem([], _, Element) :-
  Element = []
  .

getElem([Head|_], 0, Element) :-
  Element = Head
  .

getElem([_|Tail], Index, Element) :-
  Index2 is Index - 1,
  getElem(Tail, Index2, Element)
  .
