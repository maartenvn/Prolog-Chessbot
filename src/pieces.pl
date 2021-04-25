:- module(pieces, []).

:- use_module("positions").
:- use_module("moves").


%! position(+Piece, -Position)
%
%  Extract the position from the given piece.
position(piece(_, _, Position), Position).


%! type(+Piece, -Type)
%
%  Extract the type from the given piece.
type(piece(_, Type, _), Type).


%! type(+Color, -Type)
%
%  Extract the color from the given piece.
color(piece(Color, _, _), Color).


%! row_pieces(+Y, +Pieces, -RowPieces)
%
%  List of pieces for a given row.
row_pieces(Y, [Piece | Pieces], [RowPiece | RowPieces]) :- % Match
    Piece = piece:position(Piece, _/PieceY),

    % Row numbers must match
    PieceY == Y,

    % Append to the list
    RowPiece = Piece, !,

    % Recursive call
    row_pieces(Y, Pieces, RowPieces), !.

row_pieces(Y, [Piece | Pieces], RowPieces) :- % No match
    Piece = piece:position(Piece, _/PieceY),
    
    % Row numbers must not match
    PieceY \== Y,

    % Recursive call
    row_pieces(Y, Pieces, RowPieces), !.

row_pieces(_, [], []) :- !.


%! sorted_pieces/2(+Pieces, -SortedPieces)
%
%  Sorted list for a given list of pieces by X-coordinate.
sorted_pieces(Pieces, SortedPieces) :-
    sorted_pieces(Pieces, 1, SortedPieces).

%!  sorted_pieces/3(+Pieces, +X, -SortedPieces)
%
%   Helper predicate for sorted_pieces/2.
sorted_pieces(Pieces, X, [SortedPiece | SortedPieces]) :-

    % X must be valid
    between(1, 8, X),

    % Select the piece with current X coordinate, if any
    select(piece(Color, Type, X/Y), Pieces, _),

    % Add the piece
    SortedPiece = piece(Color, Type, X/Y),

    % Recursive call
    XNext is X + 1,
    sorted_pieces(Pieces, XNext, SortedPieces), !.
sorted_pieces(Pieces, X, SortedPieces) :- 

    % X must be valid
    between(1, 8, X),

    % Recursive call
    XNext is X + 1,
    sorted_pieces(Pieces, XNext, SortedPieces), !.
sorted_pieces(_, _, []) :- !.