:- module(board, [
        createBoard/5, printBoard/1, pressPoint/5

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

% pressPoint(+MineBoard,
%            +VisualBoard, +X, +Y, -NewVisualBoard)
%
% Attempts to press the given point on the given board. Returns an updated
% visual board.
pressPoint(MineBoard, VisualBoard, X, Y, NewVisualBoard) :-
  write('pressed':X:Y), nl,
  checkForMine(MineBoard, X, Y, IsMine),
  (
      IsMine == 0 ->
      write('Position is a mine!'), nl,
      replace2DElem(VisualBoard, X, Y, '*', NewVisualBoard),
      printBoard(NewVisualBoard),
      write('You Lose!'), nl,
      halt
  ;   write('Position is not a mine.'), nl,
      write("X: "), write(X), nl,
      write("Y: "), write(Y), nl,
      getNeighbors(MineBoard, X, Y, Neighbors),
      countMatch(Neighbors, "X", NumberMines),
      replace2DElem(VisualBoard, X, Y, NumberMines, NewVisualBoard)
  )
  .

% checkForMine(+MineBoard,
%              +X, +Y, -IsMine)
%
% Checks if the given position on the given board is a mine or not. Sets IsMine
% to 0 if it is a mine, or a non-zero value if it is not a mine.
checkForMine(MineBoard, X, Y, IsMine) :-
  getElem(MineBoard, X, Row),
  getElem(Row, Y, Element),
  IsMine is Element - "X"
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

% replaceElem(+InputList,
%             +Index, +NewElement, -OutputList)
%
% Replaces the element at the given index in the given list with the given new
% element.
replaceElem([], _, _, []).

replaceElem([_|T], 0, E, [E|T]).

replaceElem([H|T], N, E, OList) :-
  M is N - 1,
  replaceElem(T, M, E, OList2),
  OList = [H|OList2]
  .

% replace2DElem(+InputList
%               +X, +Y, +NewElement, -OutputList)
%
% Replaces the element at the given indexes in the given 2D list with the
% given new element.
replace2DElem([], _, _, _, []).

replace2DElem(IList, X, Y, E, OList) :-
  getElem(IList, X, Row),
  replaceElem(Row, Y, E, NewRow),
  replaceElem(IList, X, NewRow, OList)
  .

% getNeighbors(+List,
%              +X, +Y, +Neighbors)
%
% Returns a list of all of the elements around the given index position.
getNeighbors(List, X, Y, Neighbors) :-
  length(List, Rows),
  getElem(List, 0, Row1),
  length(Row1 , Columns),
  N0 = [],
  PosX0 is X - 1,
  PosY0 is Y - 1,
  tryGetElem(List, PosX0, PosY0, N0, N1),
  PosX1 is X - 1,
  PosY1 is Y,
  tryGetElem(List, PosX1, PosY1, N1, N2),
  PosX2 is X - 1,
  PosY2 is Y + 1,
  tryGetElem(List, PosX2, PosY2, N2, N3),
  PosX3 is X,
  PosY3 is Y - 1,
  tryGetElem(List, PosX3, PosY3, N3, N4),
  PosX4 is X,
  PosY4 is Y + 1,
  tryGetElem(List, PosX4, PosY4, N4, N5),
  PosX5 is X + 1,
  PosY5 is Y - 1,
  tryGetElem(List, PosX5, PosY5, N5, N6),
  PosX6 is X + 1,
  PosY6 is Y,
  tryGetElem(List, PosX6, PosY6, N6, N7),
  PosX7 is X + 1,
  PosY7 is Y + 1,
  tryGetElem(List, PosX7, PosY7, N7, N8),
  Neighbors = N8
  .

% tryGetElem(+List
%            +X, +Y, +PreList, -OutList)
tryGetElem(List, X, Y, PreList, OutList) :-
  (
      Y > -1, X > -1 ->
      nth0(0, List, Row0),
      length(List, Rows),
      length(Row0, Columns),
      (
          Y < Columns , X < Rows ->
          nth0(X, List, Row),
          nth0(Y, Row, Elem),
          OutList = [Elem|PreList]
      ;
          OutList = PreList
      )
  ;
      OutList = PreList
  )
  .

% countMatch(+List
%            +Element, -Count)
countMatch([], _, 0).

countMatch([Head|Tail], Element, Count) :-
  countMatch(Tail, Element, CountTail),
  (
      Head = Element ->
      Count is CountTail + 1
  ;
      Count = CountTail
  )
  .
