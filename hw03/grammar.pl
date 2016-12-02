:- module(grammar, [
        readCommand/3
    ]).

% A flag or press command.
%
% Ex.
%   Press position 6 2
%   Flag position 2 5
command(Type, X, Y) --> [Type, position, X, Y].

% readCommand(-Type,
%             -X, -Y)
%
% Reads in a command from the user and returns the command type and X Y
% positions.
%
% Method for reading in line found from
% https://stackoverflow.com/questions/7777991/read-line-to-atomic-list-in-prolog
readCommand(Type, X, Y) :-
  read_line_to_codes(user_input, Input),
  string_to_atom(Input, InputAtom),
  atomic_list_concat(InputList, ' ', InputAtom),
  phrase(command(Type, X, Y), InputList)
  .
