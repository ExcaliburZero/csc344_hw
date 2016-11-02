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
      replace2DElem(VisualBoard, X, Y, ' ', NewVisualBoard)
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
