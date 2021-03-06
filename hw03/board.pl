:- module(board, [
        createBoard/2, printBoard/1, pressPoint/5, flagPoint/5

        , checkForMine/4
    ]).

createBoard(MineBoard, VisualBoard) :-
  MineBoard = [
        [".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", "X", "."],
        [".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", "X", ".", ".", ".", "."],
        [".", ".", "X", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", "X", "."],
        [".", ".", ".", ".", ".", ".", ".", "."]
    ],
  VisualBoard = [
        [".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", "."]
    ]
  .

%%%%%%%%%%
% Board Printing
%%%%%%%%%%

% printBoard(+Board)
%
% Prints out the contents of the given board.
printBoard(Board) :-
  nl, printRows(Board), nl
  .

% printRows(+Rows)
%
% Prints out the rows of the given 2D list.
printRows([]).

printRows([Head|Tail]) :-
  printSingleRow(Head),
  nl,
  printRows(Tail)
  .

% printSingleRow(+Row)
%
% Prints out the contents of the given list, space separated.
printSingleRow([]).

printSingleRow([Head|Tail]) :-
  write(Head),
  write(' '),
  printSingleRow(Tail)
  .

%%%%%%%%%%
% Point Pressing
%%%%%%%%%%

% pressPoint(+MineBoard,
%            +VisualBoard, +X, +Y, -NewVisualBoard)
%
% Attempts to press the given point on the given board. Returns an updated
% visual board.
%
% Ends the game if a bomb is pressed.
pressPoint(MineBoard, VisualBoard, X, Y, NewVisualBoard) :-
  nth0(0, MineBoard, Row0),
  length(MineBoard, Rows),
  length(Row0, Columns),

  (
      X > -1 , Y > -1 , X < Rows , Y < Columns -> 
      checkForMine(MineBoard, X, Y, IsMine),
      (
          IsMine == 0 ->
          write('Position is a mine!'), nl,
          replace2DElem(VisualBoard, X, Y, '*', NewVisualBoard),
          printBoard(NewVisualBoard),
          write('You Lose!'), nl,
          halt
      ;
          write('Position is not a mine.'), nl,
          getNeighbors(MineBoard, X, Y, Neighbors),
          countMatch(Neighbors, "X", NumberMines),
          (
              NumberMines > 0 ->
              replace2DElem(VisualBoard, X, Y, NumberMines, NewVisualBoard)
          ;
              replace2DElem(VisualBoard, X, Y, "_", NewVisualBoardPre),
              clearNeighbors(MineBoard, NewVisualBoardPre, X, Y, NewVisualBoard)
          )
      )
  ;
      write("Invalid Position"), nl,
      NewVisualBoard = VisualBoard
  )
  .

%%%%%%%%%%
% Position flagging
%%%%%%%%%%

% flagPoint(+MineBoard,
%           +VisualBoard, +X, +Y, -NewVisualBoard)
%
% Attempts to flag the given point on the given board.
%
% If the position is not empty, then it will not be flagged.
%
% If all of the bombs are flagged after the flag is placed, then the game is
% won.
flagPoint(MineBoard, VisualBoard, X, Y, NewVisualBoard) :-
  nth0(0, MineBoard, Row0),
  length(MineBoard, Rows),
  length(Row0, Columns),

  (
      X > -1 , Y > -1 , X < Rows , Y < Columns ->
      nth0(X, VisualBoard, Row),
      nth0(Y, Row, Element),
      (
        Element = "." ->
        replace2DElem(VisualBoard, X, Y, "F", NewVisualBoard),
        getUnFlaggedMines(MineBoard, NewVisualBoard, Count),
        (
            Count = 0 ->
            printBoard(NewVisualBoard),
            write("You win!"), nl,
            halt
        ;
            write("")
        )
     ;
         Element = "F" ->
         replace2DElem(VisualBoard, X, Y, ".", NewVisualBoard)
     ;
         NewVisualBoard = VisualBoard
     )
  )
  .

%%%%%%%%%%
% Board Checking
%%%%%%%%%%

% checkForMine(+MineBoard,
%              +X, +Y, -IsMine)
%
% Checks if the given position on the given board is a mine or not. Sets IsMine
% to 0 if it is a mine, or a non-zero value if it is not a mine.
checkForMine(MineBoard, X, Y, IsMine) :-
  nth0(X, MineBoard, Row),
  nth0(Y, Row, Element),
  IsMine is Element - "X"
  .

% getUnFlaggedMines(+MineBoard
%                   +VisualBoard, -UnFlaggedMines)
%
% Returns the number of unflagged mines in the given Visual and Mine boards.
getUnFlaggedMines([], [], 0).

getUnFlaggedMines([MineHead|MineTail], [VisualHead|VisualTail], UnFlaggedMines) :-
  getUnFlaggedMinesRow(MineHead, VisualHead, PreCount),
  getUnFlaggedMines(MineTail, VisualTail, PostCount),
  UnFlaggedMines is PreCount + PostCount
  .

% getUnFlaggedMinesRow(+MineBoardRow,
%                      +VisualBoardRow, -UnFlaggedMines)
%
% Returns the number of unflagged mines in the given Visual and Mine rows.
getUnFlaggedMinesRow([], [], 0).

getUnFlaggedMinesRow([MineHead|MineTail], [VisualHead|VisualTail], UnFlaggedMines) :-
  (
      MineHead = "X" , VisualHead = "." ->
      Count = 1
  ;
      Count = 0
  ),
  getUnFlaggedMinesRow(MineTail, VisualTail, PreCount),
  UnFlaggedMines is Count + PreCount
  .

% tryGetElem(+List
%            +X, +Y, +PreList, -OutList)
%
% Attempts to get the element at index (X, Y) from the given board and prepends
% it to the given PreList and returns it though OutList.
%
% This predicate can be chained to collect the contents of a given board at
% multiple points.
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
%
% Counts the number of elements in the given list that are equal to the given
% Element.
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

% getElem2(+Board,
%          +X, +Y, -Elem)
%
% Returns the element in the given 2D board at the given position.
getElem2(Board, X, Y, Elem) :-
  nth0(0, Board, Row0),
  length(Board, Rows),
  length(Row0, Columns),
  (
      Y > -1 , X > -1 , X < Rows, Y < Columns ->
      nth0(X, Board, Row),
      nth0(Y, Row, Elem)
  ;
      Elem = "?"
  )
  .

%%%%%%%%%%
% Board Manipulation
%%%%%%%%%%

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
  nth0(X, IList, Row),
  replaceElem(Row, Y, E, NewRow),
  replaceElem(IList, X, NewRow, OList)
  .

%%%%%%%%%%
% Neighboring Node Handlers
%%%%%%%%%%

% getNeighbors(+List,
%              +X, +Y, +Neighbors)
%
% Returns a list of all of the elements around the given index position.
getNeighbors(List, X, Y, Neighbors) :-
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


% clearNeighbors(+MineBoard,
%                +VisualBoard, +X, +Y, -OutVisualBoard)
%
% Attempts to clear all of the neighbors of the given (X, Y) point on the given
% Mine and Visual Board, where such neghboring points are not surrounded by any
% mines.
clearNeighbors(MineBoard, VisualBoard, X, Y, OutVisualBoard) :-
  PosX0 is X - 1,
  PosY0 is Y - 1,
  handleNeighbor(MineBoard, VisualBoard, PosX0, PosY0, VisualBoard0),
  PosX1 is X - 1,
  PosY1 is Y,
  handleNeighbor(MineBoard, VisualBoard0, PosX1, PosY1, VisualBoard1),
  PosX2 is X - 1,
  PosY2 is Y + 1,
  handleNeighbor(MineBoard, VisualBoard1, PosX2, PosY2, VisualBoard2),
  PosX3 is X,
  PosY3 is Y - 1,
  handleNeighbor(MineBoard, VisualBoard2, PosX3, PosY3, VisualBoard3),
  PosX4 is X,
  PosY4 is Y + 1,
  handleNeighbor(MineBoard, VisualBoard3, PosX4, PosY4, VisualBoard4),
  PosX5 is X + 1,
  PosY5 is Y - 1,
  handleNeighbor(MineBoard, VisualBoard4, PosX5, PosY5, VisualBoard5),
  PosX6 is X + 1,
  PosY6 is Y,
  handleNeighbor(MineBoard, VisualBoard5, PosX6, PosY6, VisualBoard6),
  PosX7 is X + 1,
  PosY7 is Y + 1,
  handleNeighbor(MineBoard, VisualBoard6, PosX7, PosY7, VisualBoard7),

  OutVisualBoard = VisualBoard7
  .

% handleNeighbor(+MineBoard,
%                +VisualBoard, +X, +Y, -NewVisualBoard)
%
% Attempts to clear the given point on the given Mine and Visual boards.
handleNeighbor(MineBoard, VisualBoard, X, Y, NewVisualBoard) :-
  getElem2(VisualBoard, X, Y, Elem),
  getNeighbors(MineBoard, X, Y, Neighbors),
  countMatch(Neighbors, "X", Mines),
  (
      Elem = "." ->
      (
          Mines = 0 ->
          replace2DElem(VisualBoard, X, Y, "_", VisualBoardPre),
          clearNeighbors(MineBoard, VisualBoardPre, X, Y, NewVisualBoard)
      ;
          replace2DElem(VisualBoard, X, Y, Mines, NewVisualBoard)
      )
  ;
      NewVisualBoard = VisualBoard
  )
  .
