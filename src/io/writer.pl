:- module(writer, []).

:- use_module("parser").
:- use_module("../positions").


%! write_board(+Board, +Rokades, +StartColor)
%
%  Write a chess board to stdout.
write_board(Board, Rokades, StartColor) :-
    % Convert board & rokades in a format that could be used by the parser
    extract_rows(8, Board, Rows),
    extract_rokades(Rokades, RokadesList),

    % Parse the rows output
    parser:parse_rows(8, Rows, RokadesList, StartColor, OutRows, []),

    % Parse the final row output
    parser:parse_final_row(OutFinalRow, []),
    
    % Write the output
    write_codes(OutRows),
    write_codes(OutFinalRow).


%! write_board_moves(+StartColor, +Board, +Rokades, +Moves)
%
%  Write all possible chess boards for the given board to stdout.
write_board_moves(StartColor, Board, Rokades, [Move | Moves]) :-

    % Do the move
    positions:do_move(Move, Board, Rokades, NewBoard, NewRokades),

    % Opponent is now at play
    positions:opponent(StartColor, NextColor),

    % Write the board to stdout
    write_board(NewBoard, NewRokades, NextColor),
    write("\n~\n"),

    % Recursive call
    write_board_moves(StartColor, Board, Rokades, Moves).
write_board_moves(_, _, _, []).


%! write_codes(+Codes)
%
%  Write a given list of codes to stdout.
write_codes(Codes) :-
    atom_codes(String, Codes),
    write(String).


%! extract_rows(+Y, +Board, -Rows)
%
%  List of pieces for a given board, with each list representing the pieces for that row.
extract_rows(Y, Board, [Row | Rows]) :-
    between(1, 8, Y), !,

    % Extract the row
    positions:row_pieces(Y, Board, Row),

    % Next row
    YNext is Y - 1,

    % Recursive call
    extract_rows(YNext, Board, Rows), !.
extract_rows(_, _, []) :- !.


%! extract_rokades(+Rokades, +RokadesList)
extract_rokades(Rokades, [BlackRokades, WhiteRokades]) :-
    extract_rokades_color(black, Rokades, BlackRokades),
    extract_rokades_color(white, Rokades, WhiteRokades).


%! extract_rokades_color(+Color, +Rokades, -ColorRokades)
%
%  Extract all rokades for a given color
extract_rokades_color(Color, [Rokade | Rokades], [ColorRokade | ColorRokades]) :- % Match
    Rokade = rokade(RokadeColor, _),

    % Color must match
    Color = RokadeColor,

    % Append to the list
    ColorRokade = Rokade, !,

    % Recursive call
    extract_rokades_color(Color, Rokades, ColorRokades), !.

extract_rokades_color(Color, [Rokade | Rokades], ColorRokades) :- % No match
    Rokade = rokade(RokadeColor, _),

    % Color must not match
    Color \= RokadeColor,

    % Recursive call
    extract_rokades_color(Color, Rokades, ColorRokades), !.

extract_rokades_color(_, [], []) :- !.