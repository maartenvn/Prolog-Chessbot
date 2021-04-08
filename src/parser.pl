:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes,codes).

%! board(-Board)
%
% Parse a chess board.
board(Board) --> 
    row(Board).

%! row(-Y, -Positions)
%
% Parse a row of the chess board.
row(Positions) --> 
    row_number(Y),
    space,
    pieces(0/Y, Positions),
    newline.


%! row_number(+NumberCode)
%
% Parse a row number between 1 and 8.
row_number(NumberCode) --> 
    [Number],

    {
        number_codes(NumberCode, Number)
    }.

%! space()
%
% Parse a single space.
space --> [" "].

%! newline()
%
% Parse a single newline.
newline --> ["\n"].

%! pieces(-Pieces)
%
% Parse a row of pieces.
pieces(X/Y, []) --> [].
pieces(X/Y, [Piece|Pieces]) -->
    {
        XNext is X + 1
    },

    piece(Color, Type),
    pieces(XNext/Y, Pieces),

    {
        Piece = piece(Color, Type, XNext/Y)
    }.

%! piece(-Color, -Type)
%
% Parse a single piece.
piece(white, king)   --> ["\u2654"]. % White king
piece(white, queen)  --> ["\u2655"]. % White queen
piece(white, tower)  --> ["\u2656"]. % White tower
piece(white, bishop) --> ["\u2657"]. % White tower
piece(white, bishop) --> ["\u2658"]. % White horse
piece(white, pawn)   --> ["\u2659"]. % White pawn

piece(black, king)   --> ["\u265A"]. % Black king
piece(black, queen)  --> ["\u265B"]. % Black queen
piece(black, tower)  --> ["\u265C"]. % Black tower
piece(black, bishop) --> ["\u265D"]. % Black tower
piece(black, bishop) --> ["\u265E"]. % Black horse
piece(black, pawn)   --> ["\u265F"]. % Black pawn

piece(none, none)    --> space.      % Empty position