%%%%%%%%%%%%%%%%%%%%%%%%%
% Student Name:        Christopher Wells
% Assignment Number:   3
% Due Date:            Nov. 21, 2016
%
% Program Description:
%   Simulates the game Minesweeper with a natural language interface. Allows
%   the player to give Press or Flag commands until they either win or lose the
%   game.
%
%   Press commands will attempt to press the given (Row, Column) point. If the
%   point is empty, then it will be replaced with the number of mines that
%   surround it. If no mines surround the point, then all neighboring points
%   will also be pressed. However, if the point contains a bomb, then the user
%   loses the game.
%
%   Flag commands will attempt to toggle the given (Row, Column) point as being
%   flagged. If the point is un-revealed, then it is flagged. If the point
%   already has a flag, then the flag is removed. If once the flag is placed,
%   all of the mines have been flagged then the player wins the game.
%
% Implementation Description:
%   The game works by having two copies of the board, a MineBoard and a
%   VisualBoard. The MineBoard keeps track of where the mines are located, and
%   the VisualBoard keeps track of what the user can see and what positions
%   have been revealed or flagged. The VisualBoard is printed by recursing over
%   all of its elements and printing them out with spacing in between.
%
%   MineBoard
%
%     . . . . . . . .
%     . . . . . . * .
%     . . . . . . . .
%     . . . . . . . .
%     . . . * . . . .
%     . . * . . . . .
%     . . . . . . * .
%     . . . . . . . .
%
%   VisualBoard
%
%     _ _ _ _ _ 1 . .
%     _ _ _ _ _ 1 . .
%     _ _ _ _ _ 1 1 1
%     _ _ 1 1 1 _ _ _
%     _ 1 2 . 1 _ _ _
%     _ 1 F 2 1 1 1 1
%     _ 1 1 1 _ 1 . .
%     _ _ _ _ _ 1 . .
%
%   The program begins by creating the initial MineBoard and VisualBoard. It
%   then prompts the user for an action to perform.
%
%   A prompt asks the user to input an action command. Each action command is
%   composed of a few parts. It begins with the type of the command. Each
%   command must be either a "Press" command or a "Flag" command. Then the atom
%   position must be given. After that, the user must specify the position they
%   want to act on, giving the row and then the column.
%
%     command(Type, X, Y) --> [Type, position, X, Y]
%
%   The command is read in as a line and is then broken up into its parts.
%   Based on the Type of the command, a Press or Flag action is initiated. Once
%   the action is completed, then if the game does not end, then the user is
%   prompted to enter another command.
%
%   A Press action begins by checking if the given (Row, Column) point is valid
%   for the game board. If the point in invalid, then an error message is
%   printed and the user is reprompted to input a command.
%
%   If the position is valid, then the position is check to see if it contains
%   a mine. If the position contains a mine, then the VisualBoard is changed to
%   include the mine, a lose message is printed to the player, and the game
%   ends.
%
%   If the position does not contain a mine, then the positions around it are
%   checked to see how many of them contain mines. If one of those neighboring
%   positions are outside of the board, then they are ignored.
%
%   If the position has one or more mines in the positions surrounding it, then
%   that position is replaced on the VisualBoard with the number of mines
%   surrounding it. Then the user is prompted to enter the next command.
%
%     Visual     Mines
%     . 1 0      * . .
%     1 2 .      . . .
%     . . .      . . *
%
%   If the position does not have any mines surrounding it, then all of the
%   positions neighboring it are also pressed. Such that all connected
%   positions that are not surrounded by any mines are revealed, as well as
%   any mine-surrounded positions surrounding those non-mine surrounded
%   positions. All positions that are not surrounded by any mines are replaced
%   on the visual board with '_' characters. Once this process completes, the
%   user is then prompted to enter their next command.
%
%     Visual (B)   Mines
%     . . . .      . . . .
%     . . . .      . . . .
%     . . . .      . . . .
%     . . . .      . . . *
%
%     Visual (After pressing (0,0))
%     _ _ _ _
%     _ _ _ _
%     _ _ 1 1
%     _ _ 1 .
%
%   A Flag action begins by first checking if the given position is valid for
%   the board. If the position is invalid, then an error message is printed
%   and the user is prompted to enter another command.
%
%   If the position is valid, then the given position is checked to see if it
%   already contains a flag. If the position has already been flagged, then the
%   flag is removed, and the user is prompted to enter another command.
%
%   If the position has been pressed previously, then nothing is done to the
%   board, and the user is prompted to input another command.
%
%   If the position does not already contain a flag, and has not been pressed,
%   then a flag is placed at that position. Then all of the mine locations on
%   the MineBoard are checked to see if they have been flagged on the
%   VisualBoard. If all of the mines have been flagged, then a win message is
%   printed to the user, and the game ends.
%
%   If not all of the mines have been flagged, then the user is prompted to
%   enter the next command.
%
% Input:
%   To start the game, run the following command in the terminal:
%
%   $ make play
%
%   Once the game starts, you can enter Press or Flag commands until the game
%   finishes. Note that the commands do not need to be ended with a period.
%
%   Press(X, Y) :- Press position X Y
%   Flag (X, Y) :- Flag position X Y
%
%   Examples:
%     Press position 1 5
%     Press position 0 3
%     Flag position 6 2
%     Flag position 7 0
%
% Output:
%   Structure:
%     board
%     prompt
%     ...
%     board ending message
%
%   Ex:
%     . . . . . . . .
%     . . . . . . . .
%     . . . . . . . .
%     . . . . . . . .
%     . . . . . . . .
%     . . . . . . . .
%     . . . . . . . .
%     . . . . . . . .
%
%     |: Press position 0 0
%     Position is not a mine.
%
%     _ _ _ _ _ 1 . .
%     _ _ _ _ _ 1 . .
%     _ _ _ _ _ 1 1 1
%     _ _ 1 1 1 _ _ _
%     _ 1 2 . 1 _ _ _
%     _ 1 . 2 1 1 1 1
%     _ 1 1 1 _ 1 . .
%     _ _ _ _ _ 1 . .
%
%     |: Press position 1 6
%     Position is a mine!
%
%     _ _ _ _ _ 1 . .
%     _ _ _ _ _ 1 * .
%     _ _ _ _ _ 1 1 1
%     _ _ 1 1 1 _ _ _
%     _ 1 2 . 1 _ _ _
%     _ 1 . 2 1 1 1 1
%     _ 1 1 1 _ 1 . .
%     _ _ _ _ _ 1 . .
%
%     You Lose!
%
% Errors:
%   - If the user enters an invalid command, then the program crashes and exits
%     into swipl. In this case, the user must enter 'halt.' to exit swipl.
%
%   - If the user enters an invalid position, then an error message is printed
%     and the program continues as normal.
%
%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(minesweeper, [
        play/0
    ]).

:- use_module(board).
:- use_module(grammar).
:- initialization play.

% play()
%
% Starts the game.
play :-
  createBoard(MineBoard, VisualBoard),
  printBoard(VisualBoard),
  prompt(MineBoard, VisualBoard),
  halt
  .

% prompt(+MineBoard,
%        +VisualBoard)
%
% Prompts the player to enter a command and then processes that command.
%
% Recurses until the game ends.
prompt(MineBoard, VisualBoard) :-
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
  prompt(MineBoard, NewVisualBoard)
  .
