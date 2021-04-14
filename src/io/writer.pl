:- module(writer, []).

:- use_module("parser").
:- use_module("../positions").
:- use_module("../moves").
:- use_module("../pieces").


%! write_board(+Board, +StartColor)
%
%  Write a chess board to stdout.
write_board(Board, StartColor) :-
    Board = board(Pieces, Rokades, Passant),

    % Convert board & rokades in a format that could be used by the parser
    extract_rows(8, Pieces, PiecesList),
    extract_rokades(Rokades, RokadesList),

    % Parse the rows output
    parser:parse_rows(8, PiecesList, RokadesList, Passant, StartColor, OutRows, []),

    % Parse the final row output
    parser:parse_final_row(OutFinalRow, []),
    
    % Write the output
    write_codes(OutRows),
    write_codes(OutFinalRow).


%! write_board_moves(+StartColor, +Board, +Moves)
%
%  Write all possible chess boards for the given set of moves to stdout.
write_board_moves(StartColor, Board, [Move | Moves]) :-

    % Do the move
    moves:do_move(Move, Board, NewBoard),

    % Opponent is now at play
    positions:opponent(StartColor, NextColor),

    % Write the board to stdout
    write_board(NewBoard, NextColor),
    write("\n~\n"),

    % Recursive call
    write_board_moves(StartColor, Board, Moves).
write_board_moves(_, _, []).


%! write_codes(+Codes)
%
%  Write a given list of codes to stdout.
write_codes(Codes) :-
    atom_codes(String, Codes),
    write(String).


%! extract_rows(+Y, +Pieces, -Rows)
%
%  List of pieces for a given board, with each list representing the pieces for that row.
extract_rows(Y, Pieces, [Row | Rows]) :-

    % Y must be valid
    between(1, 8, Y), !,

    % Extract the row
    pieces:row_pieces(Y, Pieces, Row),

    % Next row
    YNext is Y - 1,

    % Recursive call
    extract_rows(YNext, Pieces, Rows), !.
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