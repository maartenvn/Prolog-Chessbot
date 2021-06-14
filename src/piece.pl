:- module(piece, []).

:- use_module("position").
:- use_module("move").


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


%! rokades_piece(+Piece, -Rokades)
%
%  List of rokades for a given piece
rokades_piece(piece(white, tower, 1/1), [rokade(white, long)]) :- !.                      % Tower
rokades_piece(piece(white, tower, 8/1), [rokade(white, short)]) :- !.                     % Tower
rokades_piece(piece(white, king, 5/1), [rokade(white, long), rokade(white, short)]) :- !. % King

rokades_piece(piece(black, tower, 1/8), [rokade(black, long)]) :- !.                      % Tower
rokades_piece(piece(black, tower, 8/8), [rokade(black, short)]) :- !.                     % Tower
rokades_piece(piece(black, king, 5/8), [rokade(black, long), rokade(black, short)]) :- !. % King

rokades_piece(_, []) :- !. % Base case


%! passant_piece(+Piece, -Passant)
%
%  En-passant possibility for a given piece
passant_piece(piece(white, pawn, X/4), passant(white, X/3)) :- !.
passant_piece(piece(black, pawn, X/5), passant(black, X/6)) :- !.
passant_piece(_, none) :- !.


%! row_pieces(+Y, +Pieces, -RowPieces)
%
%  List of pieces for a given row.
row_pieces(Y, [Piece | Pieces], [RowPiece | RowPieces]) :- % Match
    position(Piece, _/PieceY),

    % Row numbers must match
    PieceY == Y,

    % Append to the list
    RowPiece = Piece, !,

    % Recursive call
    row_pieces(Y, Pieces, RowPieces), !.

row_pieces(Y, [Piece | Pieces], RowPieces) :- % No match
    position(Piece, _/PieceY),
    
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


%! opponent(+Color, -OpponentColor)
%
%  Opponent color for a given color
opponent(white, black).
opponent(black, white).