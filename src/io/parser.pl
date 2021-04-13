:- module(parser, []).

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

test :-
    phrase_from_file(parse_board(Board), "test_chars.txt", [encoding(utf8),type(text)]),
    write(Board).

%! parse_board(-Board)
%
%  Parse a chess board.
parse_board(Board) --> 
    parse_rows(8, Board),
    parse_final_row.


%! parse_rows(-Positions)
%
%  Parse all rows in a flat list of board positions.
parse_rows(Y, Rows) -->
    {        
        YNext is Y - 1,

        % Y must be a valid Y-value
        % (this is here to allow for re-using the parser as output writer)
        between(1, 8, Y), !
    },

    % Parser the row
    parse_row(Y, Row), !,

    % Recursive call
    parse_rows(YNext, RowRest),

    % Append the row to the rows list
    % TODO: ask if this could be done without append (maybe some prolog grammer?)
    {
        append([Row, RowRest], Rows)
    }.
parse_rows(_, []) --> [].

%! parse_row(-Y, -Pieces)
%
%  Parse a row of the chess board.
parse_row(Y, Pieces) --> 
    parse_row_number(Y),
    parse_space,
    parse_pieces(1/Y, Pieces),
    parse_newline, !.


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