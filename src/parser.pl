:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).


%! parse_board(-Board)
%
% Parse a chess board.
parse_board(Board) --> 
    parse_rows(Rows),
    parse_final_row,

    {
        % Flatten the rows into a single list of positions
        flatten(Rows, Board)
    }.


%! parse_rows(-Positions)
%
% Parse all rows in a flat list of board positions.
parse_rows([Row | Rows]) -->
    parse_row(Row), !,
    parse_rows(Rows).
parse_rows([]) --> [].


%! parse_row(-Y, -Positions)
%
% Parse a row of the chess board.
parse_row(Positions) --> 
    parse_row_number(Y),
    parse_space,
    parse_pieces(0/Y, Positions),
    parse_newline.


%! parse_final_row()
%
% Parse the final row of the board.
% This row does not contain any extra information.
parse_final_row --> 
    parse_space,
    parse_space,
    "abcdefgh". 
    

%! parse_row_number(+RowNumber)
%
% Parse a row number between 1 and 8.
parse_row_number(RowNumber) --> integer(RowNumber).

%! parse_space()
%
% Parse a single space.
parse_space --> " ".


%! parse_newline()
%
% Parse a single newline.
parse_newline --> "\n".


%! parse_pieces(-Pieces)
%
% Parse a row of pieces.
% Will stop parsing when a newline is detected

% Piece: taken position
parse_pieces(X/Y, [Piece|Pieces]) -->
    {
        XNext is X + 1,
        Piece = piece(Color, Type, XNext/Y)
    },

    parse_piece(Color, Type), !,
    parse_pieces(XNext/Y, Pieces).

% Space: empty position
parse_pieces(X/Y, Pieces) -->
    {
        XNext is X + 1
    },

    parse_space,
    parse_pieces(XNext/Y, Pieces).
parse_pieces(_, []) --> [].


%! parse_piece(-Color, -Type)
%
% Parse a single piece.
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