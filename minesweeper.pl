:- initialization play.

play() :-
  write('Hello, World!'), nl,
  createBoard(number(1), number(2), Board),
  printBoard(Board),
  halt
  .
