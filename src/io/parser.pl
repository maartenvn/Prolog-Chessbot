:- module(parser, []).

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

test :-
    phrase_from_file(parse_rows(Pieces, Rokades, StartColor), "test_chars.txt", [encoding(utf8),type(text)]),
    write(Pieces),
    write(Rokades),
    write(StartColor).

%! parse_board(-Board, -Rokades, -StartColor)
%
%  Parse a chess board.
parse_board(Board, Rokades, StartColor) --> 
    parse_rows(Board, Rokades, StartColor),
    parse_final_row.


%! parse_rows(-Pieces)
%
%  Parse all rows in a flat list of board positions.
%  TODO: this could be cleaned up by a recursive statement
parse_rows(Pieces, Rokades, StartColor) -->
    parse_border_row(8, PiecesRow8, RokadesBlack, StartColor),
    parse_row(7, PiecesRow7),
    parse_row(6, PiecesRow6),
    parse_row(5, PiecesRow5),
    parse_row(4, PiecesRow4),
    parse_row(3, PiecesRow3),
    parse_row(2, PiecesRow2),
    parse_border_row(1, PiecesRow1, RokadesWhite, StartColor),
    {
        % Merge rokades
        append([RokadesBlack, RokadesWhite], Rokades),

        % Merge pieces
        append([PiecesRow1, PiecesRow2, PiecesRow3, PiecesRow4, PiecesRow5, PiecesRow6, PiecesRow7, PiecesRow8], Pieces)
    }.


%! parse_row(-Y, -Pieces)
%
%  Parse a row of the chess board.
%  Will not parse the first/last row
parse_row(Y, Pieces) --> 
    parse_row_number(Y),
    parse_space,
    parse_pieces(1/Y, Pieces),
    parse_newline, !.


%! parse_border_row(-Y, -Pieces, -Rokades, -StartColor)
%
%  Parse the first/last row of the chess board.
parse_border_row(Y, Pieces, Rokades, StartColor) -->
    parse_row_number(Y),
    parse_space,
    parse_pieces(1/Y, Pieces),
    parse_space,
    parse_rokades(Rokades),
    parse_current_player(Y, StartColor),
    parse_newline.


%! parse_rokades(-Rokades)
%
%  Parse a rokade notation.
parse_rokades(Rokades) -->
    "[",
    parse_rokade_piece(LongRokades),
    parse_rokade_piece(ShortRokades),
    {
        % Merge short & long rokades
        append([LongRokades, ShortRokades], Rokades)
    },
    "]".

%! parse_rokade_piece(-Rokades)
%
%  Parse a single piece of the rokade notation or a space.
parse_rokade_piece([Rokade]) --> % Large
    parse_piece(Color, queen),

    % Create the rokade
    {
        Rokade = rokade(Color, large)
    }, !.

parse_rokade_piece([Rokade]) --> % Short
    parse_piece(Color, king),

    % Create the rokade
    {
        Rokade = rokade(Color, small)
    }, !.

parse_rokade_piece(_) --> % Space
    parse_space, !.


%! parse_current_player(+Y, -StartColor)
%
%  Parse a current player symbol or nothing
parse_current_player(8, black) --> "☚", !.
parse_current_player(1, white) --> "☚", !.
parse_current_player(_, _)     --> "", !.


%! parse_final_row()
%
%  Parse the final row of the board.
%  This row does not contain any extra information.
parse_final_row --> 
    parse_space,
    parse_space,
    "abcdefgh". 
    

%! parse_row_number(+RowNumber)
%
%  Parse a row number between 1 and 8.
parse_row_number(RowNumber) --> integer(RowNumber).

%! parse_space()
%
%  Parse a single space.
parse_space --> " ".


%! parse_newline()
%
% Parse a single newline.
parse_newline --> "\n".


%! parse_pieces(-Pieces)
%
%  Parse a row of pieces.
%  Will stop parsing when a newline is detected

% Piece: taken position
parse_pieces(X/Y, [Piece|Pieces]) -->
    {
        XNext is X + 1,

        % Create the piece
        Piece = piece(Color, Type, X/Y),

        % X must be a valid X-value
        % (this is here to allow for re-using the parser as output writer)
        between(1, 8, X)
    },

    parse_piece(Color, Type), !,
    parse_pieces(XNext/Y, Pieces).

% Space: empty position
parse_pieces(X/Y, Pieces) -->
    {
        XNext is X + 1,

        % X must be a X-value
        % (this is here to allow for re-using the parser as output writer)
        between(1, 8, X)
    },

    parse_space,
    parse_pieces(XNext/Y, Pieces).
parse_pieces(_, []) --> [].


%! parse_piece(-Color, -Type)
%
%  Parse a single piece.
parse_piece(white, king)   --> "\u2654", !. % White king
parse_piece(white, queen)  --> "\u2655", !. % White queen
parse_piece(white, tower)  --> "\u2656", !. % White tower
parse_piece(white, bishop) --> "\u2657", !. % White tower
parse_piece(white, bishop) --> "\u2658", !. % White horse
parse_piece(white, pawn)   --> "\u2659", !. % White pawn

parse_piece(black, king)   --> "\u265A", !. % Black king
parse_piece(black, queen)  --> "\u265B", !. % Black queen
parse_piece(black, tower)  --> "\u265C", !. % Black tower
parse_piece(black, bishop) --> "\u265D", !. % Black tower
parse_piece(black, bishop) --> "\u265E", !. % Black horse
parse_piece(black, pawn)   --> "\u265F", !. % Black pawn